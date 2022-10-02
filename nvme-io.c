#include "./nvme.h"

static uint16_t nvme_io_cmd(FemuCtrl *n, NvmeCmd *cmd, NvmeRequest *req);

//更新sq的eventidx部分
static void nvme_update_sq_eventidx(const NvmeSQueue *sq)
{
    if (sq->eventidx_addr_hva) {
        *((uint32_t *)(sq->eventidx_addr_hva)) = sq->tail;
        return;
    }

    if (sq->eventidx_addr) {
        nvme_addr_write(sq->ctrl, sq->eventidx_addr, (void *)&sq->tail,
                        sizeof(sq->tail));
    }
}
//通过直接内存拷贝的方式进行
static inline void nvme_copy_cmd(NvmeCmd *dst, NvmeCmd *src)
{
#if defined(__AVX__)
    __m256i *d256 = (__m256i *)dst;
    const __m256i *s256 = (const __m256i *)src;

    _mm256_store_si256(&d256[0], _mm256_load_si256(&s256[0]));
    _mm256_store_si256(&d256[1], _mm256_load_si256(&s256[1]));
#elif defined(__SSE2__)
    __m128i *d128 = (__m128i *)dst;
    const __m128i *s128 = (const __m128i *)src;

    _mm_store_si128(&d128[0], _mm_load_si128(&s128[0]));
    _mm_store_si128(&d128[1], _mm_load_si128(&s128[1]));
    _mm_store_si128(&d128[2], _mm_load_si128(&s128[2]));
    _mm_store_si128(&d128[3], _mm_load_si128(&s128[3]));
#else
    *dst = *src;
#endif
}
//命令提交源码，用于更新门铃寄存器sq_head_db，并且生成nvme_io_cmd
//注意实际的io读写和延时模拟是分开的
static void nvme_process_sq_io(void *opaque, int index_poller)
{
    NvmeSQueue *sq = opaque;
    FemuCtrl *n = sq->ctrl;
    uint16_t status;
    hwaddr addr;
    NvmeCmd cmd;
    NvmeRequest *req;
    int processed = 0;
    //更新sq尾指针值
    nvme_update_sq_tail(sq);
    //反复从sq中取
    while (!(nvme_sq_empty(sq))) {
        if (sq->phys_contig) {  //如果物理连续存放，直接计算得到地址值
            addr = sq->dma_addr + sq->head * n->sqe_size;
            nvme_copy_cmd(&cmd, (void *)&(((NvmeCmd *)sq->dma_addr_hva)[sq->head]));
        } else {    //如果不连续存放，通过以下函数计算得到地址
            addr = nvme_discontig(sq->prp_list, sq->head, n->page_size,
                                  n->sqe_size);
            nvme_addr_read(n, addr, (void *)&cmd, sizeof(cmd)); //通过读取方式得到command
        }
        nvme_inc_sq_head(sq);   //更新头指针

        req = QTAILQ_FIRST(&sq->req_list);      //sq从req list里取出一个请求？
        QTAILQ_REMOVE(&sq->req_list, req, entry);       //从list里移出这个请求
        memset(&req->cqe, 0, sizeof(req->cqe));       //请求里的完成命令部分清0
        /* Coperd: record req->stime at earliest convenience */
        req->expire_time = req->stime = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);     //开始计时
        req->cqe.cid = cmd.cid;                    //request相关赋值操作
        req->cmd_opcode = cmd.opcode;
        memcpy(&req->cmd, &cmd, sizeof(NvmeCmd));

        if (n->print_log) {
            femu_debug("%s,cid:%d\n", __func__, cmd.cid);
        }
        //实际的数据io读写逻辑
        status = nvme_io_cmd(n, &cmd, req);
        //实际读写完成后的ftl延迟模拟，将请求入队
        if (1 && status == NVME_SUCCESS) {
            req->status = status;
            int rc = femu_ring_enqueue(n->to_ftl[index_poller], (void *)&req, 1);
            if (rc != 1) {
                femu_err("enqueue failed, ret=%d\n", rc);
            }
        } else if (status == NVME_SUCCESS) {
            /* Normal I/Os that don't need delay emulation */
            req->status = status;
        } else {
            femu_err("Error IO processed!\n");
        }

        processed++;
    }

    nvme_update_sq_eventidx(sq);        //更新eventidx寄存器
    sq->completed += processed;         //添加完成字段
}
//nvme 完成队列加入命令完成消息
static void nvme_post_cqe(NvmeCQueue *cq, NvmeRequest *req)
{
    FemuCtrl *n = cq->ctrl;
    NvmeSQueue *sq = req->sq;
    NvmeCqe *cqe = &req->cqe;
    uint8_t phase = cq->phase;  //相位域
    hwaddr addr;

    if (n->print_log) {
        femu_debug("%s,req,lba:%lu,lat:%lu\n", n->devname, req->slba, req->reqlat);
    }
    cqe->status = cpu_to_le16((req->status << 1) | phase);
    cqe->sq_id = cpu_to_le16(sq->sqid);
    cqe->sq_head = cpu_to_le16(sq->head);

    if (cq->phys_contig) {      //如果物理连续，取得尾槽的地址
        addr = cq->dma_addr + cq->tail * n->cqe_size;
        ((NvmeCqe *)cq->dma_addr_hva)[cq->tail] = *cqe;
    } else {
        addr = nvme_discontig(cq->prp_list, cq->tail, n->page_size, n->cqe_size);
        nvme_addr_write(n, addr, (void *)cqe, sizeof(*cqe));
    }
    //更新cq tail部分，并且修改相位字段
    nvme_inc_cq_tail(cq);
}

//处理一个cq条目的过程，并且处理真正的物理时延模拟
static void nvme_process_cq_cpl(void *arg, int index_poller)
{
    FemuCtrl *n = (FemuCtrl *)arg;
    NvmeCQueue *cq = NULL;
    NvmeRequest *req = NULL;
    struct rte_ring *rp = n->to_ftl[index_poller];
    pqueue_t *pq = n->pq[index_poller];
    uint64_t now;
    int processed = 0;
    int rc;
    //bb会单独处理成poller？
    if (BBSSD(n)) { //|| ZNSSD(n)
        rp = n->to_poller[index_poller];
    }

    while (femu_ring_count(rp)) {
        req = NULL;
        rc = femu_ring_dequeue(rp, (void *)&req, 1);
        if (rc != 1) {
            femu_err("dequeue from to_poller request failed\n");
        }
        assert(req);    //断言指针非空
        //加入到对应的pqueue中
        pqueue_insert( pq, req);
    }
    //取出优先级最高的request，注意，没有pop
    while ((req = pqueue_peek(pq))) {
        now = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);   //获取当前时间
        if (now < req->expire_time) {   //还没到结束时间
            break;
        }

        cq = n->cq[req->sq->sqid];
        if (!cq->is_active)
            continue;
        //时延结束后向对应的cq加入完成命令消息
        nvme_post_cqe(cq, req);
        QTAILQ_INSERT_TAIL(&req->sq->req_list, req, entry);
        //pq弹出本请求
        pqueue_pop(pq);
        processed++;    //处理数+1
        n->nr_tt_ios++;
        if (now - req->expire_time >= 20000) {
            n->nr_tt_late_ios++;
            if (n->print_log) {
                femu_debug("%s,diff,pq.count=%lu,%" PRId64 ", %lu/%lu\n",
                           n->devname, pqueue_size(pq), now - req->expire_time,
                           n->nr_tt_late_ios, n->nr_tt_ios);
            }
        }

        n->should_isr[req->sq->sqid] = true;    //本队列应该产生中断
    }

    if (processed == 0)
        return;

    switch (n->multipoller_enabled) {
    case 1:
        nvme_isr_notify_io(n->cq[index_poller]);
        break;
    default:
        for (int i = 1; i <= n->num_io_queues; i++) {
            if (n->should_isr[i]) {
                nvme_isr_notify_io(n->cq[i]);
                n->should_isr[i] = false;
            }
        }
        break;
    }
}
//nvme poller线程执行的函数，参数为NVmePollerThreadArgument，在poller里实际进行sq和cq的处理
static void *nvme_poller(void *arg)
{
    FemuCtrl *n = ((NvmePollerThreadArgument *)arg)->n;
    int index = ((NvmePollerThreadArgument *)arg)->index;
    int i=0;
    //根据是否支持多poller
    switch (n->multipoller_enabled) {
    case 1:
        while (1) {
            if ((!n->dataplane_started)) {
                usleep(1000);
                continue;
            }
            i++;
            NvmeSQueue *sq = n->sq[index];
            NvmeCQueue *cq = n->cq[index];
            if (sq && sq->is_active && cq && cq->is_active) {
                nvme_process_sq_io(sq, index);
            }
            nvme_process_cq_cpl(n, index);
        }
        break;
    default:
        while (1) {
            if ((!n->dataplane_started)) {
                usleep(1000);
                continue;
            }
            i++;

            for (int i = 1; i <= n->num_io_queues; i++) {
                NvmeSQueue *sq = n->sq[i];
                NvmeCQueue *cq = n->cq[i];
                if (sq && sq->is_active && cq && cq->is_active) {
                    nvme_process_sq_io(sq, index);
                }
            }
            nvme_process_cq_cpl(n, index);
        }
        break;
    }

    return NULL;
}

static int cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
{
    return (next > curr);
}

static pqueue_pri_t get_pri(void *a)
{
    return ((NvmeRequest *)a)->expire_time;
}

static void set_pri(void *a, pqueue_pri_t pri)
{
    ((NvmeRequest *)a)->expire_time = pri;
}

static size_t get_pos(void *a)
{
    return ((NvmeRequest *)a)->pos;
}

static void set_pos(void *a, size_t pos)
{
    ((NvmeRequest *)a)->pos = pos;
}


//创建poller函数，用于生成需要使用到的poller
void nvme_create_poller(FemuCtrl *n)
{
    //为每个queue申请一个bool变量
    n->should_isr = g_malloc0(sizeof(bool) * (n->num_io_queues + 1));
    //是否允许多poller，如果是则每个队列一个;否则共用一个
    n->num_poller = n->multipoller_enabled ? n->num_io_queues : 1;
    /* Coperd: we put NvmeRequest into these rings */
    //根据实际的poller数量，为to_ftl和to_poller申请rte_ring
    n->to_ftl = malloc(sizeof(struct rte_ring *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->to_ftl[i] = femu_ring_create(FEMU_RING_TYPE_MP_SC, FEMU_MAX_INF_REQS);
        if (!n->to_ftl[i]) {    //创建失败
            femu_err("failed to create ring (n->to_ftl) ...\n");
            abort();
        }
        assert(rte_ring_empty(n->to_ftl[i]));
    }
    //与to_ftl的创建过程类似
    n->to_poller = malloc(sizeof(struct rte_ring *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->to_poller[i] = femu_ring_create(FEMU_RING_TYPE_MP_SC, FEMU_MAX_INF_REQS);
        if (!n->to_poller[i]) {
            femu_err("failed to create ring (n->to_poller) ...\n");
            abort();
        }
        assert(rte_ring_empty(n->to_poller[i]));
    }
    //创建pq指针数组，同样类似于上面
    n->pq = malloc(sizeof(pqueue_t *) * (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        n->pq[i] = pqueue_init(FEMU_MAX_INF_REQS, cmp_pri, get_pri, set_pri,
                               get_pos, set_pos);
        if (!n->pq[i]) {
            femu_err("failed to create pqueue (n->pq) ...\n");
            abort();
        }
    }
    //创建poller，poller数量同样与num_poller保持一致
    n->poller = malloc(sizeof(QemuThread) * (n->num_poller + 1));
    NvmePollerThreadArgument *args = malloc(sizeof(NvmePollerThreadArgument) *
                                            (n->num_poller + 1));
    for (int i = 1; i <= n->num_poller; i++) {
        args[i].n = n;
        args[i].index = i;
        qemu_thread_create(&n->poller[i], "nvme-poller", nvme_poller, &args[i],
                           QEMU_THREAD_JOINABLE);
        femu_debug("nvme-poller [%d] created ...\n", i - 1);
    }
}
//nvme实际的读写操作
uint16_t nvme_rw(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd, NvmeRequest *req)
{
    //获取参数信息
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint16_t ctrl = le16_to_cpu(rw->control);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint64_t prp1 = le64_to_cpu(rw->prp1);
    uint64_t prp2 = le64_to_cpu(rw->prp2);
    //获取lba_index
    const uint8_t lba_index = NVME_ID_NS_FLBAS_INDEX(ns->id_ns.flbas);
    //获得指定格式的ms部分
    const uint16_t ms = le16_to_cpu(ns->id_ns.lbaf[lba_index].ms);
    //获得指定格式的data size部分，即每个数据块的大小（字节单位）
    const uint8_t data_shift = ns->id_ns.lbaf[lba_index].lbads;
    //计算数据大小
    uint64_t data_size = (uint64_t)nlb << data_shift;
    //计算byte地址
    uint64_t data_offset = slba << data_shift;
    uint64_t meta_size = nlb * ms;
    uint64_t elba = slba + nlb;
    uint16_t err;
    int ret;
    //判定是否是写请求
    req->is_write = (rw->opcode == NVME_CMD_WRITE) ? 1 : 0;
    //检查参数合理性，如果返回非0值，则失败
    err = femu_nvme_rw_check_req(n, ns, cmd, req, slba, elba, nlb, ctrl,
                                 data_size, meta_size);
    if (err)
        return err;

    //prp映射，通过prp获取dma传输的数据块地址。如果返回值非0，进入错误处理阶段
    if (nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, data_size, n)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, prp1), 0, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    //断言检查
    assert((nlb << data_shift) == req->qsg.size);
    //赋值request部分
    req->slba = slba;
    req->status = NVME_SUCCESS;
    req->nlb = nlb;
    //交付给backend memory进行实际的读写
    ret = backend_rw(n->mbe, &req->qsg, &data_offset, req->is_write);
    if (!ret) {
        return NVME_SUCCESS;
    }

    return NVME_DNR;
}

static uint16_t nvme_dsm(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                         NvmeRequest *req)
{
    uint32_t dw10 = le32_to_cpu(cmd->cdw10);
    uint32_t dw11 = le32_to_cpu(cmd->cdw11);
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);

    if (dw11 & NVME_DSMGMT_AD) {
        uint16_t nr = (dw10 & 0xff) + 1;

        uint64_t slba;
        uint32_t nlb;
        NvmeDsmRange range[nr];

        if (dma_write_prp(n, (uint8_t *)range, sizeof(range), prp1, prp2)) {
            nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                                offsetof(NvmeCmd, dptr.prp1), 0, ns->id);
            return NVME_INVALID_FIELD | NVME_DNR;
        }

        req->status = NVME_SUCCESS;
        for (int i = 0; i < nr; i++) {
            slba = le64_to_cpu(range[i].slba);
            nlb = le32_to_cpu(range[i].nlb);
            if (slba + nlb > le64_to_cpu(ns->id_ns.nsze)) {
                nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                                    offsetof(NvmeCmd, cdw10), slba + nlb, ns->id);
                return NVME_LBA_RANGE | NVME_DNR;
            }

            bitmap_clear(ns->util, slba, nlb);
        }
    }

    return NVME_SUCCESS;
}

static uint16_t nvme_compare(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                             NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint64_t prp1 = le64_to_cpu(rw->prp1);
    uint64_t prp2 = le64_to_cpu(rw->prp2);

    uint64_t elba = slba + nlb;
    uint8_t lba_index = NVME_ID_NS_FLBAS_INDEX(ns->id_ns.flbas);
    uint8_t data_shift = ns->id_ns.lbaf[lba_index].lbads;
    uint64_t data_size = nlb << data_shift;
    uint64_t offset  = ns->start_block + (slba << data_shift);

    if ((slba + nlb) > le64_to_cpu(ns->id_ns.nsze)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), elba, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }
    if (n->id_ctrl.mdts && data_size > n->page_size * (1 << n->id_ctrl.mdts)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, nlb), nlb, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, data_size, n)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, prp1), 0, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (find_next_bit(ns->uncorrectable, elba, slba) < elba) {
        return NVME_UNRECOVERED_READ;
    }

    for (int i = 0; i < req->qsg.nsg; i++) {
        uint32_t len = req->qsg.sg[i].len;
        uint8_t tmp[2][len];

        nvme_addr_read(n, req->qsg.sg[i].base, tmp[1], len);
        if (memcmp(tmp[0], tmp[1], len)) {
            qemu_sglist_destroy(&req->qsg);
            return NVME_CMP_FAILURE;
        }
        offset += len;
    }

    qemu_sglist_destroy(&req->qsg);

    return NVME_SUCCESS;
}

//Nvme flush操作，未做实现
static uint16_t nvme_flush(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                           NvmeRequest *req)
{
    return NVME_SUCCESS;
}

static uint16_t nvme_write_zeros(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                                 NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;

    if ((slba + nlb) > ns->id_ns.nsze) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), slba + nlb, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }

    return NVME_SUCCESS;
}

static uint16_t nvme_write_uncor(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                                 NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb  = le16_to_cpu(rw->nlb) + 1;

    if ((slba + nlb) > ns->id_ns.nsze) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), slba + nlb, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }

    bitmap_set(ns->uncorrectable, slba, nlb);

    return NVME_SUCCESS;
}
//处理nvme io指令
static uint16_t nvme_io_cmd(FemuCtrl *n, NvmeCmd *cmd, NvmeRequest *req)
{
    NvmeNamespace *ns;
    uint32_t nsid = le32_to_cpu(cmd->nsid);

    if (nsid == 0 || nsid > n->num_namespaces) {
        femu_err("%s, NVME_INVALID_NSID %" PRIu32 "\n", __func__, nsid);
        return NVME_INVALID_NSID | NVME_DNR;
    }

    req->ns = ns = &n->namespaces[nsid - 1];

    switch (cmd->opcode) {
    case NVME_CMD_FLUSH:

        if (!n->id_ctrl.vwc || !n->features.volatile_wc) {
            return NVME_SUCCESS;
        }
        return nvme_flush(n, ns, cmd, req);
    case NVME_CMD_DSM:

        if (NVME_ONCS_DSM & n->oncs) {
            return nvme_dsm(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_COMPARE:

        if (NVME_ONCS_COMPARE & n->oncs) {
            return nvme_compare(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_WRITE_ZEROES:

        if (NVME_ONCS_WRITE_ZEROS & n->oncs) {
            return nvme_write_zeros(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    case NVME_CMD_WRITE_UNCOR:

        if (NVME_ONCS_WRITE_UNCORR & n->oncs) {
            return nvme_write_uncor(n, ns, cmd, req);
        }
        return NVME_INVALID_OPCODE | NVME_DNR;
    default:
        //其他默认情况走了io_cmd里自定义的操作，注意这里包含了读写
        if (n->ext_ops.io_cmd) {
            return n->ext_ops.io_cmd(n, ns, cmd, req);
        }

        femu_err("%s, NVME_INVALID_OPCODE\n", __func__);
        return NVME_INVALID_OPCODE | NVME_DNR;
    }
}

void nvme_post_cqes_io(void *opaque)
{
    NvmeCQueue *cq = opaque;
    NvmeRequest *req, *next;
    int64_t cur_time, ntt = 0;  //NTT?
    int processed = 0;

    QTAILQ_FOREACH_SAFE(req, &cq->req_list, entry, next) {
        if (nvme_cq_full(cq)) {
            break;
        }

        cur_time = qemu_clock_get_ns(QEMU_CLOCK_REALTIME);
        if (cq->cqid != 0 && cur_time < req->expire_time) {
            ntt = req->expire_time;
            break;
        }

        nvme_post_cqe(cq, req);
        processed++;
    }

    if (ntt == 0) {
        ntt = qemu_clock_get_ns(QEMU_CLOCK_REALTIME) + CQ_POLLING_PERIOD_NS;
    }

    /* Only interrupt guest when we "do" complete some I/Os */
    if (processed > 0) {
        nvme_isr_notify_io(cq);
    }
}
