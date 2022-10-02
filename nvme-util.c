#include "./nvme.h"
//检查sqid是否超过了FemuCtrl规定的队列数量，且对应id槽是否非空
int nvme_check_sqid(FemuCtrl *n, uint16_t sqid)
{
    return sqid <= n->num_io_queues && n->sq[sqid] != NULL ? 0 : -1;
}
//检查cqid是否超过了FemuCtrl规定的队列数量，且对应id槽是否非空
int nvme_check_cqid(FemuCtrl *n, uint16_t cqid)
{
    return cqid <= n->num_io_queues && n->cq[cqid] != NULL ? 0 : -1;
}

void nvme_inc_cq_tail(NvmeCQueue *cq)
{
    cq->tail++;
    if (cq->tail >= cq->size) {
        cq->tail = 0;
        cq->phase = !cq->phase;
    }
}

void nvme_inc_sq_head(NvmeSQueue *sq)
{
    sq->head = (sq->head + 1) % sq->size;
}

//从doorbell寄存器中获取最新的尾指针
void nvme_update_sq_tail(NvmeSQueue *sq)
{
    if (sq->db_addr_hva) {
        sq->tail = *((uint32_t *)sq->db_addr_hva);
        return;
    }

    if (sq->db_addr) {
        nvme_addr_read(sq->ctrl, sq->db_addr, &sq->tail, sizeof(sq->tail));
    }
}

void nvme_update_cq_head(NvmeCQueue *cq)
{
    if (cq->db_addr_hva) {
        cq->head = *(uint32_t *)(cq->db_addr_hva);
        return;
    }

    if (cq->db_addr) {
        nvme_addr_read(cq->ctrl, cq->db_addr, &cq->head, sizeof(cq->head));
    }
}

uint8_t nvme_cq_full(NvmeCQueue *cq)
{
    nvme_update_cq_head(cq);

    return (cq->tail + 1) % cq->size == cq->head;
}

uint8_t nvme_sq_empty(NvmeSQueue *sq)
{
    return sq->head == sq->tail;
}

uint64_t *nvme_setup_discontig(FemuCtrl *n, uint64_t prp_addr, uint16_t
                               queue_depth, uint16_t entry_size)
{
    uint16_t prps_per_page = n->page_size >> 3;
    uint64_t prp[prps_per_page];
    uint16_t total_prps = DIV_ROUND_UP(queue_depth * entry_size, n->page_size);
    uint64_t *prp_list = g_malloc0(total_prps * sizeof(*prp_list));

    for (int i = 0; i < total_prps; i++) {
        if (i % prps_per_page == 0 && i < total_prps - 1) {
            if (!prp_addr || prp_addr & (n->page_size - 1)) {
                g_free(prp_list);
                return NULL;
            }
            nvme_addr_write(n, prp_addr, (uint8_t *)&prp, sizeof(prp));
            prp_addr = le64_to_cpu(prp[prps_per_page - 1]);
        }
        prp_list[i] = le64_to_cpu(prp[i % prps_per_page]);
        if (!prp_list[i] || prp_list[i] & (n->page_size - 1)) {
            g_free(prp_list);
            return NULL;
        }
    }

    return prp_list;
}

void nvme_set_error_page(FemuCtrl *n, uint16_t sqid, uint16_t cid, uint16_t
                         status, uint16_t location, uint64_t lba, uint32_t nsid)
{
    NvmeErrorLog *elp;

    elp = &n->elpes[n->elp_index];
    elp->error_count = n->error_count++;
    elp->sqid = sqid;
    elp->cid = cid;
    elp->status_field = status;
    elp->param_error_location = location;
    elp->lba = lba;
    elp->nsid = nsid;
    n->elp_index = (n->elp_index + 1) % n->elpe;
    ++n->num_errors;
}


//检查rw请求中的参数合理性
uint16_t femu_nvme_rw_check_req(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                                NvmeRequest *req, uint64_t slba, uint64_t elba,
                                uint32_t nlb, uint16_t ctrl, uint64_t data_size,
                                uint64_t meta_size)
{

    if (elba > le64_to_cpu(ns->id_ns.nsze)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_LBA_RANGE,
                            offsetof(NvmeRwCmd, nlb), elba, ns->id);
        return NVME_LBA_RANGE | NVME_DNR;
    }
    if (n->id_ctrl.mdts && data_size > n->page_size * (1 << n->id_ctrl.mdts)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, nlb), nlb, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (meta_size) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, control), ctrl, ns->id);
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if ((ctrl & NVME_RW_PRINFO_PRACT) && !(ns->id_ns.dps & DPS_TYPE_MASK)) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_INVALID_FIELD,
                            offsetof(NvmeRwCmd, control), ctrl, ns->id);
        /* Not contemplated in LightNVM for now */
        if (OCSSD(n)) {
            return 0;
        }
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (!req->is_write && find_next_bit(ns->uncorrectable, elba, slba) < elba) {
        nvme_set_error_page(n, req->sq->sqid, cmd->cid, NVME_UNRECOVERED_READ,
                            offsetof(NvmeRwCmd, slba), elba, ns->id);
        return NVME_UNRECOVERED_READ;
    }

    return 0;
}

void nvme_free_sq(NvmeSQueue *sq, FemuCtrl *n)
{
    n->sq[sq->sqid] = NULL;
    g_free(sq->io_req);
    if (sq->prp_list) {
        g_free(sq->prp_list);
    }
    if (sq->sqid) {
        g_free(sq);
    }
}
//初始化一个提交队列，基本与cq完全相同
uint16_t nvme_init_sq(NvmeSQueue *sq, FemuCtrl *n, uint64_t dma_addr, uint16_t
                      sqid, uint16_t cqid, uint16_t size, enum NvmeQueueFlags
                      prio, int contig)
{
    uint8_t stride = n->db_stride;
    int dbbuf_entry_sz = 1 << (2 + stride);
    AddressSpace *as = pci_get_address_space(&n->parent_obj);
    dma_addr_t sqsz = (dma_addr_t)size;
    NvmeCQueue *cq;

    sq->ctrl = n;
    sq->sqid = sqid;
    sq->size = size;
    sq->cqid = cqid;
    sq->head = sq->tail = 0;
    sq->phys_contig = contig;
    if (sq->phys_contig) {
        sq->dma_addr = dma_addr;
        sq->dma_addr_hva = (uint64_t)dma_memory_map(as, dma_addr, &sqsz, 0, MEMTXATTRS_UNSPECIFIED);
    } else {
        sq->prp_list = nvme_setup_discontig(n, dma_addr, size, n->sqe_size);
        if (!sq->prp_list) {
            return NVME_INVALID_FIELD | NVME_DNR;
        }
    }
    //与cq不同的地方，申请了nvme请求数组的空间
    sq->io_req = g_malloc0(sq->size * sizeof(*sq->io_req));
    //注意与cq不同的地方在于队列的类型有所不同
    QTAILQ_INIT(&sq->req_list);
    QTAILQ_INIT(&sq->out_req_list);
    //并且进行了相关元素的初始化
    for (int i = 0; i < sq->size; i++) {
        sq->io_req[i].sq = sq;  //每个nvmerequest关联sq
        QTAILQ_INSERT_TAIL(&(sq->req_list), &sq->io_req[i], entry); //并且插入到req_list队列尾部，entry是连接的指针的字段名
    }
    //优先级处理，根据不同的优先级设置arb_burst仲裁字段
    switch (prio) {
    case NVME_Q_PRIO_URGENT:
        sq->arb_burst = (1 << NVME_ARB_AB(n->features.arbitration));
        break;
    case NVME_Q_PRIO_HIGH:
        sq->arb_burst = NVME_ARB_HPW(n->features.arbitration) + 1;
        break;
    case NVME_Q_PRIO_NORMAL:
        sq->arb_burst = NVME_ARB_MPW(n->features.arbitration) + 1;
        break;
    case NVME_Q_PRIO_LOW:
    default:
        sq->arb_burst = NVME_ARB_LPW(n->features.arbitration) + 1;
        break;
    }
    //与cq相似的db和ei设置
    if (sqid && n->dbs_addr && n->eis_addr) {
        sq->db_addr = n->dbs_addr + 2 * sqid * dbbuf_entry_sz;
        sq->db_addr_hva = n->dbs_addr_hva + 2 * sqid * dbbuf_entry_sz;
        sq->eventidx_addr = n->eis_addr + 2 * sqid * dbbuf_entry_sz;
        sq->eventidx_addr = n->eis_addr_hva + 2 * sqid + dbbuf_entry_sz;
        femu_debug("SQ[%d],db=%" PRIu64 ",ei=%" PRIu64 "\n", sqid, sq->db_addr,
                sq->eventidx_addr);
    }

    assert(n->cq[cqid]);
    //在相关的cq的sq_list中插入新sq
    cq = n->cq[cqid];
    QTAILQ_INSERT_TAIL(&(cq->sq_list), sq, entry);
    //设置槽位
    n->sq[sqid] = sq;

    return NVME_SUCCESS;
}
//nvme初始化一个完成队列
uint16_t nvme_init_cq(NvmeCQueue *cq, FemuCtrl *n, uint64_t dma_addr, uint16_t
                      cqid, uint16_t vector, uint16_t size, uint16_t
                      irq_enabled, int contig)
{
    cq->ctrl = n;
    cq->cqid = cqid;
    cq->size = size;
    cq->phase = 1;
    cq->irq_enabled = irq_enabled;
    cq->vector = vector;
    cq->head = cq->tail = 0;
    cq->phys_contig = contig;
    //doorbell步长
    uint8_t stride = n->db_stride;
    //默认情况紧密排布时长度为4，所以2+
    int dbbuf_entry_sz = 1 << (2 + stride);
    //获取地址空间
    AddressSpace *as = pci_get_address_space(&n->parent_obj);
    dma_addr_t cqsz = (dma_addr_t)size;

    //物理连续
    if (cq->phys_contig) {
        cq->dma_addr = dma_addr;
        cq->dma_addr_hva = (uint64_t)dma_memory_map(as, dma_addr, &cqsz, 1, MEMTXATTRS_UNSPECIFIED);
    } else {//不连续
        cq->prp_list = nvme_setup_discontig(n, dma_addr, size,
                n->cqe_size);
        if (!cq->prp_list) {
            return NVME_INVALID_FIELD | NVME_DNR;
        }
    }
    //初始化两个队列
    QTAILQ_INIT(&cq->req_list);
    QTAILQ_INIT(&cq->sq_list);
    //doorbell相关设置
    if (cqid && n->dbs_addr && n->eis_addr) {
        cq->db_addr = n->dbs_addr + (2 * cqid + 1) * dbbuf_entry_sz;        //因为SQ和CQ紧密排布，所以取+1，同时也可以知cqid和sqid独立计算
        cq->db_addr_hva = n->dbs_addr_hva + (2 * cqid + 1) * dbbuf_entry_sz;
        cq->eventidx_addr = n->eis_addr + (2 * cqid + 1) * dbbuf_entry_sz;
        cq->eventidx_addr_hva = n->eis_addr_hva + (2 * cqid + 1) * dbbuf_entry_sz;
        femu_debug("CQ, db_addr=%" PRIu64 ", eventidx_addr=%" PRIu64 "\n",
                cq->db_addr, cq->eventidx_addr);
    }
    msix_vector_use(&n->parent_obj, cq->vector);
    //置于槽内
    n->cq[cqid] = cq;

    return NVME_SUCCESS;
}

//释放掉某一个完成队列
void nvme_free_cq(NvmeCQueue *cq, FemuCtrl *n)
{
    //置空
    n->cq[cq->cqid] = NULL;
    msix_vector_unuse(&n->parent_obj, cq->vector);
    //释放prp空间
    if (cq->prp_list) {
        g_free(cq->prp_list);
    }
    //释放cq本体
    if (cq->cqid) {
        g_free(cq);
    }
}

void nvme_set_ctrl_name(FemuCtrl *n, const char *mn, const char *sn, int *dev_id)
{
    NvmeIdCtrl *id = &n->id_ctrl;
    char *subnqn;
    char serial[MN_MAX_LEN], dev_id_str[ID_MAX_LEN];

    memset(serial, 0, MN_MAX_LEN);
    memset(dev_id_str, 0, ID_MAX_LEN);
    strcat(serial, sn);

    sprintf(dev_id_str, "%d", *dev_id);
    strcat(serial, dev_id_str);
    (*dev_id)++;
    strpadcpy((char *)id->mn, sizeof(id->mn), mn, ' ');

    memset(n->devname, 0, MN_MAX_LEN);
    g_strlcpy(n->devname, serial, sizeof(serial));

    strpadcpy((char *)id->sn, sizeof(id->sn), serial, ' ');
    strpadcpy((char *)id->fr, sizeof(id->fr), "1.0", ' ');

    subnqn = g_strdup_printf("nqn.2021-05.org.femu:%s", serial);
    strpadcpy((char *)id->subnqn, sizeof(id->subnqn), subnqn, '\0');
}

