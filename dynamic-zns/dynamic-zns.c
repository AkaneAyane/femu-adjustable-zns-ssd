#include "./dynamic-zns.h"
#include <math.h>
#define MIN_DISCARD_GRANULARITY     (16 * KiB)      //页大小？
#define NVME_DEFAULT_ZONE_SIZE      (128 * MiB)     //zone大小
#define NVME_DEFAULT_MAX_AZ_SIZE    (128 * KiB)     //
uint64_t lag = 0;

// 通过slba获取zone id ,需要修改的关键函数之一
// slba ssd logical block address
// slpa ssd logical page address
static inline uint32_t zns_zone_idx(NvmeNamespace *ns, uint64_t slba)
{
    FemuCtrl *n = ns->ctrl;
    //zone_size  以512B大小扇区为单位=262144(128M)
    return (n->zone_size_log2 > 0 ? slba >> n->zone_size_log2 :
            slba / n->zone_size);// slba 除以 zone_size直接就是zone id
}

//根据slba获取所对应的zone，需要修改的关键函数之一
static inline NvmeZone *zns_get_zone_by_slba(NvmeNamespace *ns, uint64_t slba)
{
    FemuCtrl *n = ns->ctrl;
    uint32_t zone_idx = zns_zone_idx(ns, slba); //
    assert(zone_idx < n->num_zones);
    return &n->zone_array[zone_idx];    //获得zone，
}

//初始化zone布局 0成功 -1失败
static int zns_init_zone_geometry(NvmeNamespace *ns, Error **errp)
{
    FemuCtrl *n = ns->ctrl;
    uint64_t zone_size, zone_cap;
    uint32_t lbasz = 1 << zns_ns_lbads(ns); //扇区大小
    //printf( "lbasz = %d \n",lbasz);//add qwj
    if (n->zone_size_bs) {
        zone_size = n->zone_size_bs;
    } else {
        zone_size = NVME_DEFAULT_ZONE_SIZE;
    }

    if (n->zone_cap_bs) {
        zone_cap = n->zone_cap_bs;
    } else {
        zone_cap = zone_size;
    }

    if (zone_cap > zone_size) {
        femu_err("zone capacity %luB > zone size %luB", zone_cap, zone_size);
        return -1;
    }
    if (zone_size < lbasz) {
        //zone 的大小至少要超过一个扇区的大小
        femu_err("zone size %luB too small, must >= %uB", zone_size, lbasz);
        return -1;
    }
    if (zone_cap < lbasz) {
        //zone 的cap至少要超过一个扇区的大小
        femu_err("zone capacity %luB too small, must >= %uB", zone_cap, lbasz);
        return -1;
    }

    n->zone_size = zone_size / lbasz;                   //计算扇区块为单位的size
    n->zone_capacity = zone_cap / lbasz;                //计算扇区块为单位的capacity
    n->num_zones = ns->size / lbasz / n->zone_size;     //计算namespace中的zone个数

    if (n->max_open_zones > n->num_zones) {     //最大的打开的zone个数不超过zone总数量
        femu_err("max_open_zones value %u exceeds the number of zones %u",
                 n->max_open_zones, n->num_zones);
        return -1;
    }
    if (n->max_active_zones > n->num_zones) {   //最多的active zone数量不超过zone总数量
        femu_err("max_active_zones value %u exceeds the number of zones %u",
                 n->max_active_zones, n->num_zones);
        return -1;
    }

    if (n->zd_extension_size) {     //如果存在拓展空间
        if (n->zd_extension_size & 0x3f) {      //最低限度
            femu_err("zone descriptor extension size must be multiples of 64B");
            return -1;
        }
        if ((n->zd_extension_size >> 6) > 0xff) {   //最大限度
            femu_err("zone descriptor extension size is too large");
            return -1;
        }
    }
    return 0;
}

//初始化一个zone的状态
static void zns_init_zoned_state(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    uint64_t start = 0, zone_size = n->zone_size;
    //namespace的总逻辑块能力
    uint64_t capacity = n->num_zones * zone_size;

    NvmeZone *zone;
    uint32_t i;
    n->zone_array = g_new0(NvmeZone, n->num_zones);
    if (n->zd_extension_size) {
        //申请拓展空间
        n->zd_extensions = g_malloc0(n->zd_extension_size * n->num_zones);
    }

    QTAILQ_INIT(&n->exp_open_zones);    //
    QTAILQ_INIT(&n->imp_open_zones);
    QTAILQ_INIT(&n->closed_zones);
    QTAILQ_INIT(&n->full_zones);

    zone = n->zone_array;
    for (i = 0; i < n->num_zones; i++, zone++) {
        //使用实际的capacity调整实际的zone_size
        if (start + zone_size > capacity) {
            zone_size = capacity - start;
        }
        zone->d.zt = NVME_ZONE_TYPE_SEQ_WRITE;
#if MK_ZONE_CONVENTIONAL
        if( (i & (UINT32_MAX << MK_ZONE_CONVENTIONAL)) == 0){
            zone->d.zt = NVME_ZONE_TYPE_CONVENTIONAL;}
#endif
        zns_set_zone_state(zone, NVME_ZONE_STATE_EMPTY);
        zone->d.za = 0;
        zone->d.zcap = n->zone_capacity;    //固定zone大小设置
        zone->d.zslba = start;
        zone->d.wp = start;
        zone->w_ptr = start;
        start += zone_size;
    }
    //取zone_size以2为底的对数
    n->zone_size_log2 = 0;
    if (is_power_of_2(n->zone_size)) {
        n->zone_size_log2 = 63 - clz64(n->zone_size);   // 18 = 63 - 45  // 11= 63 - 52
    }
}

//初始化ns中的一些变量以及NvmeIdNsZoned信息
static void  zns_init_zone_identify(FemuCtrl *n, NvmeNamespace *ns, int lba_index)
{
    NvmeIdNsZoned *id_ns_z;
    zns_init_zoned_state(ns);

    id_ns_z = g_malloc0(sizeof(NvmeIdNsZoned));

    /* MAR/MOR are zeroes-based, 0xffffffff means no limit */
    id_ns_z->mar = cpu_to_le32(n->max_active_zones - 1);
    id_ns_z->mor = cpu_to_le32(n->max_open_zones - 1);
    id_ns_z->zoc = 0;   //如果做可变的话，可能需要改
    id_ns_z->ozcs = n->cross_zone_read ? 0x01 : 0x00;   //一次读操作是否可以跨zone读取

    id_ns_z->lbafe[lba_index].zsze = cpu_to_le64(n->zone_size);
    id_ns_z->lbafe[lba_index].zdes = n->zd_extension_size >> 6; /* Units of 64B */

    n->csi = NVME_CSI_ZONED;
    ns->id_ns.nsze = cpu_to_le64(n->num_zones * n->zone_size);
    ns->id_ns.ncap = ns->id_ns.nsze;
    ns->id_ns.nuse = ns->id_ns.ncap;

    /* NvmeIdNs */
    /*
     * The device uses the BDRV_BLOCK_ZERO flag to determine the "deallocated"
     * status of logical blocks. Since the specification defines that logical
     * blocks SHALL be deallocated when then zone is in the Empty or Offline states,
     * we can only support DULBE（deallocated or unwritten logical blocks error)
     * if the zone size is a multiple of the calculated NPDG.
     */
    if (n->zone_size % (ns->id_ns.npdg + 1)) {
        femu_err("the zone size (%"PRIu64" blocks) is not a multiple of the"
                 "calculated deallocation granularity (%"PRIu16" blocks); DULBE"
                 "support disabled", n->zone_size, ns->id_ns.npdg + 1);
        ns->id_ns.nsfeat &= ~0x4;   //NSFEAT& 1011,用于关闭DAE功能
    }

    n->id_ns_zoned = id_ns_z;
}
//清理zone，调用的前提是d.wp已经还原成了w_ptr？暂时不理解
static void zns_clear_zone(NvmeNamespace *ns, NvmeZone *zone)
{
    FemuCtrl *n = ns->ctrl;
    uint8_t state;
    zone->w_ptr = zone->d.wp;
    state = zns_get_zone_state(zone);
    if (zone->d.wp != zone->d.zslba ||
        (zone->d.za & NVME_ZA_ZD_EXT_VALID)) {      //如果有一个关联的zone descriptor extension data
        if (state != NVME_ZONE_STATE_CLOSED) {      //先关闭
            zns_set_zone_state(zone, NVME_ZONE_STATE_CLOSED);
        }
        zns_aor_inc_active(ns);
        QTAILQ_INSERT_HEAD(&n->closed_zones, zone, entry);  //加入到关闭队列
    } else {
        zns_set_zone_state(zone, NVME_ZONE_STATE_EMPTY);    //直接设置为空
    }
}
//关闭一个zoned模式的namespace
static void zns_zoned_ns_shutdown(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *zone, *next;

    QTAILQ_FOREACH_SAFE(zone, &n->closed_zones, entry, next) {
        QTAILQ_REMOVE(&n->closed_zones, zone, entry);
        zns_aor_dec_active(ns);
        zns_clear_zone(ns, zone);
    }
    QTAILQ_FOREACH_SAFE(zone, &n->imp_open_zones, entry, next) {
        QTAILQ_REMOVE(&n->imp_open_zones, zone, entry);
        zns_aor_dec_open(ns);
        zns_aor_dec_active(ns);
        zns_clear_zone(ns, zone);
    }
    QTAILQ_FOREACH_SAFE(zone, &n->exp_open_zones, entry, next) {
        QTAILQ_REMOVE(&n->exp_open_zones, zone, entry);
        zns_aor_dec_open(ns);
        zns_aor_dec_active(ns);
        zns_clear_zone(ns, zone);
    }

    assert(n->nr_open_zones == 0);
}
//关闭一个namespace
void zns_ns_shutdown(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;

    if (n->zoned) {
        zns_zoned_ns_shutdown(ns);
    }
}

//清理一个namespace
void zns_ns_cleanup(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    //释放与zone直接相关的结构体内存
    if (n->zoned) {
        g_free(n->id_ns_zoned);
        g_free(n->zone_array);
        g_free(n->zd_extensions);
    }
}
//给zone分配一个state
static void zns_assign_zone_state(NvmeNamespace *ns, NvmeZone *zone,
                                  NvmeZoneState state)
{
    FemuCtrl *n = ns->ctrl;

    if (QTAILQ_IN_USE(zone, entry)) {
        switch (zns_get_zone_state(zone)) {
        case NVME_ZONE_STATE_EXPLICITLY_OPEN:
            QTAILQ_REMOVE(&n->exp_open_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_IMPLICITLY_OPEN:
            QTAILQ_REMOVE(&n->imp_open_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_CLOSED:
            QTAILQ_REMOVE(&n->closed_zones, zone, entry);
            break;
        case NVME_ZONE_STATE_FULL:
            QTAILQ_REMOVE(&n->full_zones, zone, entry);
        default:
            ;
        }
    }

    zns_set_zone_state(zone, state);

    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        QTAILQ_INSERT_TAIL(&n->exp_open_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        QTAILQ_INSERT_TAIL(&n->imp_open_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_CLOSED:
        QTAILQ_INSERT_TAIL(&n->closed_zones, zone, entry);
        break;
    case NVME_ZONE_STATE_FULL:
        QTAILQ_INSERT_TAIL(&n->full_zones, zone, entry);
    case NVME_ZONE_STATE_READ_ONLY:
        break;
    default:
        zone->d.za = 0;
    }
}

/*
 * AOR stands for "Active and Open Resources" (see TP 4053 section 2.5).
 * 检查是否可以打开或者激活一个zone，且不超过open/active上限)
 */
static int zns_aor_check(NvmeNamespace *ns, uint32_t act, uint32_t opn)
{
    FemuCtrl *n = ns->ctrl;

    if (n->max_active_zones != 0 &&
        n->nr_active_zones + act > n->max_active_zones) {
        return NVME_ZONE_TOO_MANY_ACTIVE | NVME_DNR;
    }
    if (n->max_open_zones != 0 &&
        n->nr_open_zones + opn > n->max_open_zones) {
        return NVME_ZONE_TOO_MANY_OPEN | NVME_DNR;
    }

    return NVME_SUCCESS;
}
//检查zone的状态是否可写
static uint16_t zns_check_zone_state_for_write(NvmeZone *zone)
{
    uint16_t status;

    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_EMPTY:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_CLOSED:
        status = NVME_SUCCESS;
        break;
    case NVME_ZONE_STATE_FULL:
        status = NVME_ZONE_FULL;
        break;
    case NVME_ZONE_STATE_OFFLINE:
        status = NVME_ZONE_OFFLINE;
        break;
    case NVME_ZONE_STATE_READ_ONLY:
        status = NVME_ZONE_READ_ONLY;
        break;
    default:
        assert(false);
    }

    return status;
}
//检查zone是否满足本write请求的条件
static uint16_t zns_check_zone_write(FemuCtrl *n, NvmeNamespace *ns,
                                      NvmeZone *zone, uint64_t slba,
                                      uint32_t nlb, bool append)
{
    uint16_t status;
    uint32_t zidx = zns_zone_idx(ns, slba);
    if (unlikely((slba + nlb) > zns_zone_wr_boundary(zone))) {  //先检查write边界
        status = NVME_ZONE_BOUNDARY_ERROR;
    } else {
        status = zns_check_zone_state_for_write(zone);          //检查状态是否可写
    }

    if (status != NVME_SUCCESS) {
    } else {
        assert(zns_wp_is_valid(zone));              //断言检查写指针状态
        if (append) {
            if (unlikely(slba != zone->d.zslba)) {  //必须从zslba开始写？
                //Zone Start Logical Block Address
                status = NVME_INVALID_FIELD;
            }
            if (zns_l2b(ns, nlb) > (n->page_size << n->zasl)) {     //逻辑块对应地字节数不应该超过单次可写的最大值
                status = NVME_INVALID_FIELD;
            }
            if((zidx == 0) || (zidx == 1) || (zidx == 2) || (zidx == 3)){
                femu_err("[inho] zns.c:406 append wp error(%d) in zidx=%d",status, zidx);
            }
        } else if (unlikely(slba != zone->w_ptr)) {                 //检查slba满足追加写/顺序写

            status = NVME_ZONE_INVALID_WRITE;   
#if MK_ZONE_CONVENTIONAL
            if((zidx & (UINT32_MAX << MK_ZONE_CONVENTIONAL))==0){
                //zidx & (UINT32_MAX << 3) == 0 //2^3 convs
                //(zidx == 0) || (zidx == 1) || (zidx == 2) || (zidx == 3)
                //NVME_ZONE_TYPE_CONVENTIONAL;
                zone->w_ptr = slba;
                status = NVME_SUCCESS;
            }
            //zone->w_ptr = zone->d.zslba;
#endif
        }
    }
    return status;
}
//检查zone状态是否可读
static uint16_t zns_check_zone_state_for_read(NvmeZone *zone)
{
    uint16_t status;
    //只有offline状态会失效
    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_EMPTY:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_FULL:
    case NVME_ZONE_STATE_CLOSED:
    case NVME_ZONE_STATE_READ_ONLY:
        status = NVME_SUCCESS;
        break;
    case NVME_ZONE_STATE_OFFLINE:
        status = NVME_ZONE_OFFLINE;
        break;
    default:
        assert(false);
    }

    return status;
}
//检查zone是否满足本read请求的条件
static uint16_t zns_check_zone_read(NvmeNamespace *ns, uint64_t slba,
                                    uint32_t nlb)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *zone = zns_get_zone_by_slba(ns, slba);    //根据slba获取所对应的zone ，需要修改的关键函数之一
    uint64_t bndry = zns_zone_rd_boundary(ns, zone);    //当前zone的右边界
    uint64_t end = slba + nlb;                          //需要读的逻辑块结束地址
    uint16_t status;
    //检查zone是否可读
    status = zns_check_zone_state_for_read(zone);
    if (status != NVME_SUCCESS) {
        ;
    } else if (unlikely(end > bndry)) { //如果返回NVME_SUCCESS进一步判断，end是否越过zone范围
        if (!n->cross_zone_read) {  //如果不允许跨zone读取
            status = NVME_ZONE_BOUNDARY_ERROR;
        } else {
            /*
             * Read across zone boundary - check that all subsequent
             * zones that are being read have an appropriate state.
             */
            do {
                zone++;     //如果允许跨zone，那么循环读取到最后的边界，且需要保证每一个zone都是可读
                status = zns_check_zone_state_for_read(zone);
                if (status != NVME_SUCCESS) {
                    break;
                }
            } while (end > zns_zone_rd_boundary(ns, zone));
        }
    }

    return status;
}
//zns状态转置
static void zns_auto_transition_zone(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *zone;

    if (n->max_open_zones &&
        n->nr_open_zones == n->max_open_zones) {    //如果当前打开的zone已经达到上限
        zone = QTAILQ_FIRST(&n->imp_open_zones);    //获取隐式open的zone队列的第一个zone
        if (zone) {                                 //如果存在，那么将其关闭
             /* Automatically close this implicitly open zone */
            QTAILQ_REMOVE(&n->imp_open_zones, zone, entry);         //从队列中移除
            zns_aor_dec_open(ns);   //减少open个数
            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);    //将该zone的状态改为关闭
        }
    }
}
//自动打开zone
static uint16_t zns_auto_open_zone(NvmeNamespace *ns, NvmeZone *zone)
{
    uint16_t status = NVME_SUCCESS;
    uint8_t zs = zns_get_zone_state(zone);

    if (zs == NVME_ZONE_STATE_EMPTY) {      //处于empty状态时
        zns_auto_transition_zone(ns);
        status = zns_aor_check(ns, 1, 1);
    } else if (zs == NVME_ZONE_STATE_CLOSED) {      //处于closed状态时,处于已激活但是还没打开？
        zns_auto_transition_zone(ns);
        status = zns_aor_check(ns, 0, 1);
    }

    return status;
}
//write完成后的处理
static void zns_finalize_zoned_write(NvmeNamespace *ns, NvmeRequest *req,
                                     bool failed)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    NvmeZone *zone;
    NvmeZonedResult *res = (NvmeZonedResult *)&req->cqe;
    uint64_t slba;
    uint32_t nlb;

    slba = le64_to_cpu(rw->slba);
    nlb = le16_to_cpu(rw->nlb) + 1;
    zone = zns_get_zone_by_slba(ns, slba);

    zone->d.wp += nlb;          //注意这里是d里面的，在zns_write中有一个对zone里的w_ptr+

    if (failed) {
        res->slba = 0;
    }
    //写满的处理
    if (zone->d.wp == zns_zone_wr_boundary(zone)) {
        switch (zns_get_zone_state(zone)) {
        case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        case NVME_ZONE_STATE_EXPLICITLY_OPEN:
            zns_aor_dec_open(ns);
            /* fall through */
        case NVME_ZONE_STATE_CLOSED:
            zns_aor_dec_active(ns);
            /* fall through */
        case NVME_ZONE_STATE_EMPTY:
            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_FULL);
            /* fall through */
        case NVME_ZONE_STATE_FULL:
            break;
        default:
            assert(false);
        }
    }
}
//移动zone写指针
static uint64_t zns_advance_zone_wp(NvmeNamespace *ns, NvmeZone *zone,
                                    uint32_t nlb)
{
    uint64_t result = zone->w_ptr;
    uint8_t zs;
    //pthread_spin_lock(&ns->ctrl->pci_lock);
    zone->w_ptr += nlb;
    //pthread_spin_unlock(&ns->ctrl->pci_lock);
    if (zone->w_ptr < zns_zone_wr_boundary(zone)) { //状态合理时，更新状态
        zs = zns_get_zone_state(zone);
        switch (zs) {
        case NVME_ZONE_STATE_EMPTY:
            zns_aor_inc_active(ns);
            /* fall through */
        case NVME_ZONE_STATE_CLOSED:
            zns_aor_inc_open(ns);
            zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_IMPLICITLY_OPEN);
        }
    }

    return result;
}

struct zns_zone_reset_ctx {
    NvmeRequest *req;
    NvmeZone    *zone;
};
//重置zone空间，没有理解aio体现在哪里
static void zns_aio_zone_reset_cb(NvmeRequest *req, NvmeZone *zone)
{
    NvmeNamespace *ns = req->ns;

    /* FIXME, We always assume reset SUCCESS */
    switch (zns_get_zone_state(zone)) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        /* fall through */
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
        /* fall through */
    case NVME_ZONE_STATE_CLOSED:
        zns_aor_dec_active(ns);
        /* fall through */
    case NVME_ZONE_STATE_FULL:
        zone->w_ptr = zone->d.zslba;
        zone->d.wp = zone->w_ptr;
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_EMPTY);
    default:
        break;
    }
}

//处理函数
typedef uint16_t (*op_handler_t)(NvmeNamespace *, NvmeZone *, NvmeZoneState,
                                 NvmeRequest *);

//NVMe处理掩码
enum NvmeZoneProcessingMask {
    NVME_PROC_CURRENT_ZONE    = 0,
    NVME_PROC_OPENED_ZONES    = 1 << 0,
    NVME_PROC_CLOSED_ZONES    = 1 << 1,
    NVME_PROC_READ_ONLY_ZONES = 1 << 2,
    NVME_PROC_FULL_ZONES      = 1 << 3,
};
//打开一个zone
static uint16_t zns_open_zone(NvmeNamespace *ns, NvmeZone *zone,
                              NvmeZoneState state, NvmeRequest *req)
{
    uint16_t status;

    switch (state) {                //switch中没有break，连续执行直到显式打开
    case NVME_ZONE_STATE_EMPTY:
        status = zns_aor_check(ns, 1, 0);
        if (status != NVME_SUCCESS) {
            return status;
        }
        zns_aor_inc_active(ns);
        /* fall through */
    case NVME_ZONE_STATE_CLOSED:
        status = zns_aor_check(ns, 0, 1);
        if (status != NVME_SUCCESS) {
            if (state == NVME_ZONE_STATE_EMPTY) {
                zns_aor_dec_active(ns);
            }
            return status;
        }
        zns_aor_inc_open(ns);
        /* fall through */
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_EXPLICITLY_OPEN);
        /* fall through */
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}
//关闭一个zone
static uint16_t zns_close_zone(NvmeNamespace *ns, NvmeZone *zone,
                               NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        /* fall through */
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);
        /* fall through */
    case NVME_ZONE_STATE_CLOSED:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}
//完成一个zone
static uint16_t zns_finish_zone(NvmeNamespace *ns, NvmeZone *zone,
                                NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        /* fall through */
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
        zns_aor_dec_open(ns);
        /* fall through */
    case NVME_ZONE_STATE_CLOSED:
        zns_aor_dec_active(ns);
        /* fall through */
    case NVME_ZONE_STATE_EMPTY:
        zone->w_ptr = zns_zone_wr_boundary(zone);
        zone->d.wp = zone->w_ptr;
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_FULL);
        /* fall through */
    case NVME_ZONE_STATE_FULL:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}
//重置一个zone
static uint16_t zns_reset_zone(NvmeNamespace *ns, NvmeZone *zone,
                               NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_EMPTY:
        return NVME_SUCCESS;
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_CLOSED:
    case NVME_ZONE_STATE_FULL:
        break;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }

    zns_aio_zone_reset_cb(req, zone);

    return NVME_SUCCESS;
}

//offline一个zone
static uint16_t zns_offline_zone(NvmeNamespace *ns, NvmeZone *zone,
                                 NvmeZoneState state, NvmeRequest *req)
{
    switch (state) {
    case NVME_ZONE_STATE_READ_ONLY:
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_OFFLINE);
        /* fall through */
    case NVME_ZONE_STATE_OFFLINE:
        return NVME_SUCCESS;
    default:
        return NVME_ZONE_INVAL_TRANSITION;
    }
}

//zone是空状态时，设置zone descriptor extension
static uint16_t zns_set_zd_ext(NvmeNamespace *ns, NvmeZone *zone)
{
    uint16_t status;
    uint8_t state = zns_get_zone_state(zone);

    if (state == NVME_ZONE_STATE_EMPTY) {
        status = zns_aor_check(ns, 1, 0);
        if (status != NVME_SUCCESS) {
            return status;
        }
        zns_aor_inc_active(ns);
        zone->d.za |= NVME_ZA_ZD_EXT_VALID;     //将za.zdev设置为1
        zns_assign_zone_state(ns, zone, NVME_ZONE_STATE_CLOSED);
        return NVME_SUCCESS;
    }

    return NVME_ZONE_INVAL_TRANSITION;
}

//zone操作掩码设置，在当前proc_mask上进行与操作
static uint16_t zns_bulk_proc_zone(NvmeNamespace *ns, NvmeZone *zone,
                                   enum NvmeZoneProcessingMask proc_mask,
                                   op_handler_t op_hndlr, NvmeRequest *req)
{
    uint16_t status = NVME_SUCCESS;
    NvmeZoneState zs = zns_get_zone_state(zone);
    bool proc_zone;

    switch (zs) {
    case NVME_ZONE_STATE_IMPLICITLY_OPEN:
    case NVME_ZONE_STATE_EXPLICITLY_OPEN:
        proc_zone = proc_mask & NVME_PROC_OPENED_ZONES;
        break;
    case NVME_ZONE_STATE_CLOSED:
        proc_zone = proc_mask & NVME_PROC_CLOSED_ZONES;
        break;
    case NVME_ZONE_STATE_READ_ONLY:
        proc_zone = proc_mask & NVME_PROC_READ_ONLY_ZONES;
        break;
    case NVME_ZONE_STATE_FULL:
        proc_zone = proc_mask & NVME_PROC_FULL_ZONES;
        break;
    default:
        proc_zone = false;
    }

    if (proc_zone) {
        status = op_hndlr(ns, zone, zs, req);
    }

    return status;
}

//zone操作执行
static uint16_t zns_do_zone_op(NvmeNamespace *ns, NvmeZone *zone,
                               enum NvmeZoneProcessingMask proc_mask,
                               op_handler_t op_hndlr, NvmeRequest *req)
{
    FemuCtrl *n = ns->ctrl;
    NvmeZone *next;
    uint16_t status = NVME_SUCCESS;
    int i;

    if (!proc_mask) {
        status = op_hndlr(ns, zone, zns_get_zone_state(zone), req);
    } else {
        if (proc_mask & NVME_PROC_CLOSED_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->closed_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
        if (proc_mask & NVME_PROC_OPENED_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->imp_open_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }

            QTAILQ_FOREACH_SAFE(zone, &n->exp_open_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
        if (proc_mask & NVME_PROC_FULL_ZONES) {
            QTAILQ_FOREACH_SAFE(zone, &n->full_zones, entry, next) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }

        if (proc_mask & NVME_PROC_READ_ONLY_ZONES) {
            for (i = 0; i < n->num_zones; i++, zone++) {
                status = zns_bulk_proc_zone(ns, zone, proc_mask, op_hndlr,
                                             req);
                if (status && status != NVME_NO_COMPLETE) {
                    goto out;
                }
            }
        }
    }

out:
    return status;
}

static uint16_t zns_get_mgmt_zone_slba_idx(FemuCtrl *n, NvmeCmd *c,
                                           uint64_t *slba, uint32_t *zone_idx)
{
    NvmeNamespace *ns = &n->namespaces[0];
    uint32_t dw10 = le32_to_cpu(c->cdw10);
    uint32_t dw11 = le32_to_cpu(c->cdw11);

    if (!n->zoned) {
        return NVME_INVALID_OPCODE | NVME_DNR;
    }

    *slba = ((uint64_t)dw11) << 32 | dw10;
    if (unlikely(*slba >= ns->id_ns.nsze)) {
        *slba = 0;
        return NVME_LBA_RANGE | NVME_DNR;
    }

    *zone_idx = zns_zone_idx(ns, *slba);
    assert(*zone_idx < n->num_zones);

    return NVME_SUCCESS;
}
//zone 管理命令执行
static uint16_t zns_zone_mgmt_send(FemuCtrl *n, NvmeRequest *req)
{
    NvmeCmd *cmd = (NvmeCmd *)&req->cmd;
    NvmeNamespace *ns = req->ns;
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);
    NvmeZone *zone;
    uintptr_t *resets;
    uint8_t *zd_ext;
    uint32_t dw13 = le32_to_cpu(cmd->cdw13);
    uint64_t slba = 0;
    uint32_t zone_idx = 0;
    uint16_t status;
    uint8_t action;
    bool all;
    enum NvmeZoneProcessingMask proc_mask = NVME_PROC_CURRENT_ZONE;

    action = dw13 & 0xff;
    all = dw13 & 0x100;

    req->status = NVME_SUCCESS;

    if (!all) {
        status = zns_get_mgmt_zone_slba_idx(n, cmd, &slba, &zone_idx);
        if (status) {
            return status;
        }
    }

    zone = &n->zone_array[zone_idx];
    if (slba != zone->d.zslba) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    switch (action) {
    case NVME_ZONE_ACTION_OPEN:
        if (all) {
            proc_mask = NVME_PROC_CLOSED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_open_zone, req);
        break;
    case NVME_ZONE_ACTION_CLOSE:
        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_close_zone, req);
        break;
    case NVME_ZONE_ACTION_FINISH:
        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES | NVME_PROC_CLOSED_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_finish_zone, req);
        break;
    case NVME_ZONE_ACTION_RESET:
        resets = (uintptr_t *)&req->opaque;

        if (all) {
            proc_mask = NVME_PROC_OPENED_ZONES | NVME_PROC_CLOSED_ZONES |
                NVME_PROC_FULL_ZONES;
        }
        *resets = 1;
        status = zns_do_zone_op(ns, zone, proc_mask, zns_reset_zone, req);
        /*在io路径上直接计算物理时延模拟，暂时取消
        req->expire_time += zns_advance_status(n, ns, cmd, req);
         */
        (*resets)--;
        return NVME_SUCCESS;
    case NVME_ZONE_ACTION_OFFLINE:
        if (all) {
            proc_mask = NVME_PROC_READ_ONLY_ZONES;
        }
        status = zns_do_zone_op(ns, zone, proc_mask, zns_offline_zone, req);
        break;
    case NVME_ZONE_ACTION_SET_ZD_EXT:
        if (all || !n->zd_extension_size) {
            return NVME_INVALID_FIELD | NVME_DNR;
        }
        zd_ext = zns_get_zd_extension(ns, zone_idx);
        status = dma_write_prp(n, (uint8_t *)zd_ext, n->zd_extension_size, prp1,
                               prp2);
        if (status) {
            return status;
        }
        status = zns_set_zd_ext(ns, zone);
        if (status == NVME_SUCCESS) {
            return status;
        }
        break;
    default:
        status = NVME_INVALID_FIELD;
    }

    if (status) {
        status |= NVME_DNR;
    }

    return status;
}

static bool zns_zone_matches_filter(uint32_t zafs, NvmeZone *zl)
{
    NvmeZoneState zs = zns_get_zone_state(zl);

    switch (zafs) {
    case NVME_ZONE_REPORT_ALL:
        return true;
    case NVME_ZONE_REPORT_EMPTY:
        return zs == NVME_ZONE_STATE_EMPTY;
    case NVME_ZONE_REPORT_IMPLICITLY_OPEN:
        return zs == NVME_ZONE_STATE_IMPLICITLY_OPEN;
    case NVME_ZONE_REPORT_EXPLICITLY_OPEN:
        return zs == NVME_ZONE_STATE_EXPLICITLY_OPEN;
    case NVME_ZONE_REPORT_CLOSED:
        return zs == NVME_ZONE_STATE_CLOSED;
    case NVME_ZONE_REPORT_FULL:
        return zs == NVME_ZONE_STATE_FULL;
    case NVME_ZONE_REPORT_READ_ONLY:
        return zs == NVME_ZONE_STATE_READ_ONLY;
    case NVME_ZONE_REPORT_OFFLINE:
        return zs == NVME_ZONE_STATE_OFFLINE;
    default:
        return false;
    }
}

static uint16_t zns_zone_mgmt_recv(FemuCtrl *n, NvmeRequest *req)
{
    NvmeCmd *cmd = (NvmeCmd *)&req->cmd;
    NvmeNamespace *ns = req->ns;
    uint64_t prp1 = le64_to_cpu(cmd->dptr.prp1);
    uint64_t prp2 = le64_to_cpu(cmd->dptr.prp2);
    /* cdw12 is zero-based number of dwords to return. Convert to bytes */
    uint32_t data_size = (le32_to_cpu(cmd->cdw12) + 1) << 2;
    uint32_t dw13 = le32_to_cpu(cmd->cdw13);
    uint32_t zone_idx, zra, zrasf, partial;
    uint64_t max_zones, nr_zones = 0;
    uint16_t status;
    uint64_t slba, capacity = zns_ns_nlbas(ns);
    NvmeZoneDescr *z;
    NvmeZone *zone;
    NvmeZoneReportHeader *header;
    void *buf, *buf_p;
    size_t zone_entry_sz;

    req->status = NVME_SUCCESS;

    status = zns_get_mgmt_zone_slba_idx(n, cmd, &slba, &zone_idx);
    if (status) {
        return status;
    }

    zra = dw13 & 0xff;
    if (zra != NVME_ZONE_REPORT && zra != NVME_ZONE_REPORT_EXTENDED) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }
    if (zra == NVME_ZONE_REPORT_EXTENDED && !n->zd_extension_size) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    zrasf = (dw13 >> 8) & 0xff;
    if (zrasf > NVME_ZONE_REPORT_OFFLINE) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    if (data_size < sizeof(NvmeZoneReportHeader)) {
        return NVME_INVALID_FIELD | NVME_DNR;
    }

    status = nvme_check_mdts(n, data_size);
    if (status) {
        return status;
    }

    partial = (dw13 >> 16) & 0x01;

    zone_entry_sz = sizeof(NvmeZoneDescr);
    if (zra == NVME_ZONE_REPORT_EXTENDED) {
        zone_entry_sz += n->zd_extension_size;
    }

    max_zones = (data_size - sizeof(NvmeZoneReportHeader)) / zone_entry_sz;
    buf = g_malloc0(data_size);

    zone = &n->zone_array[zone_idx];
    for (; slba < capacity; slba += n->zone_size) {
        if (partial && nr_zones >= max_zones) {
            break;
        }
        if (zns_zone_matches_filter(zrasf, zone++)) {
            nr_zones++;
        }
    }
    header = (NvmeZoneReportHeader *)buf;
    header->nr_zones = cpu_to_le64(nr_zones);

    buf_p = buf + sizeof(NvmeZoneReportHeader);
    for (; zone_idx < n->num_zones && max_zones > 0; zone_idx++) {
        zone = &n->zone_array[zone_idx];
        if (zns_zone_matches_filter(zrasf, zone)) {
            z = (NvmeZoneDescr *)buf_p;
            buf_p += sizeof(NvmeZoneDescr);

            z->zt = zone->d.zt;
            z->zs = zone->d.zs;
            z->zcap = cpu_to_le64(zone->d.zcap);
            z->zslba = cpu_to_le64(zone->d.zslba);
            z->za = zone->d.za;

            if (zns_wp_is_valid(zone)) {
                z->wp = cpu_to_le64(zone->d.wp);
            } else {
                z->wp = cpu_to_le64(~0ULL);
            }

            if (zra == NVME_ZONE_REPORT_EXTENDED) {
                if (zone->d.za & NVME_ZA_ZD_EXT_VALID) {
                    memcpy(buf_p, zns_get_zd_extension(ns, zone_idx),
                           n->zd_extension_size);
                }
                buf_p += n->zd_extension_size;
            }

            max_zones--;
        }
    }

    status = dma_read_prp(n, (uint8_t *)buf, data_size, prp1, prp2);

    g_free(buf);

    return status;
}

static inline bool nvme_csi_has_nvm_support(NvmeNamespace *ns)
{
    switch (ns->ctrl->csi) {
    case NVME_CSI_NVM:
    case NVME_CSI_ZONED:
        return true;
    }
    return false;
}
//检查start lba及nlb的合理关系
static inline uint16_t zns_check_bounds(NvmeNamespace *ns, uint64_t slba,
                                        uint32_t nlb)
{
    uint64_t nsze = le64_to_cpu(ns->id_ns.nsze);

    if (unlikely(UINT64_MAX - slba < nlb || slba + nlb > nsze)) {
        return NVME_LBA_RANGE | NVME_DNR;
    }

    return NVME_SUCCESS;
}

//映射到dptr
static uint16_t zns_map_dptr(FemuCtrl *n, size_t len, NvmeRequest *req)
{
    uint64_t prp1, prp2;

    switch (req->cmd.psdt) {
    case NVME_PSDT_PRP:     //判断prp类型如果是普通的prp，俺么直接取出prp1和prp2地址，并且完成解析
        prp1 = le64_to_cpu(req->cmd.dptr.prp1);
        prp2 = le64_to_cpu(req->cmd.dptr.prp2);

        return nvme_map_prp(&req->qsg, &req->iov, prp1, prp2, len, n);
    default:
        return NVME_INVALID_FIELD;
    }
}

static uint16_t zns_do_write(FemuCtrl *n, NvmeRequest *req, bool append,
                             bool wrz)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    NvmeNamespace *ns = req->ns;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb = (uint32_t)le16_to_cpu(rw->nlb) + 1;
    uint64_t data_size = zns_l2b(ns, nlb);
    uint64_t data_offset;
    NvmeZone *zone;
    NvmeZonedResult *res = (NvmeZonedResult *)&req->cqe;
    uint16_t status;

    if (!wrz) {
        status = nvme_check_mdts(n, data_size);
        if (status) {
            goto err;
        }
    }

    status = zns_check_bounds(ns, slba, nlb);
    if (status) {
        goto err;
    }

    zone = zns_get_zone_by_slba(ns, slba);

    status = zns_check_zone_write(n, ns, zone, slba, nlb, append);
    if (status) {
        goto err;
    }

    status = zns_auto_open_zone(ns, zone);
    if (status) {
        goto err;
    }

    if (append) {
        slba = zone->w_ptr;
    }

    res->slba = zns_advance_zone_wp(ns, zone, nlb);

    data_offset = zns_l2b(ns, slba);

    if (!wrz) {
        status = zns_map_dptr(n, data_size, req);
        if (status) {
            goto err;
        }
        /*在IO路径上直接进行时延模拟，暂时取消，从而在线程中统一进行
        req->expire_time += zns_advance_status(n,ns,&req->cmd,req);
        */
        backend_rw(n->mbe, &req->qsg, &data_offset, req->is_write);
    }

    zns_finalize_zoned_write(ns, req, false);
    return NVME_SUCCESS;

err:
    printf("****************Append Failed***************\n");
    return status | NVME_DNR;
}

static uint16_t zns_admin_cmd(FemuCtrl *n, NvmeCmd *cmd)
{

    switch (cmd->opcode) {
    default:
        return NVME_INVALID_OPCODE | NVME_DNR;
    }
}

static inline uint16_t zns_zone_append(FemuCtrl *n, NvmeRequest *req)
{
    return zns_do_write(n, req, true, false);
}
//检查dulbe状态，没有实现
static uint16_t zns_check_dulbe(NvmeNamespace *ns, uint64_t slba, uint32_t nlb)
{
    return NVME_SUCCESS;
}

//zns推进延迟函数,类似FTL.c中的延迟推进，只涉及到模拟时延迟的推进，而不涉及到真正的物理时延
static int zns_advance_status(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd, NvmeRequest *req){
    
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    uint8_t opcode = rw->opcode;
    uint32_t dw13 = le32_to_cpu(cmd->cdw13);
    uint8_t action;
    //add qwj
    struct zns_ssdparams * spp = &n->zns->sp; 
    zns_ssd_lun *chip = NULL;
    uint64_t nand_stime =0;
    uint64_t cmd_stime = (req->stime == 0) ? qemu_clock_get_ns(QEMU_CLOCK_REALTIME) : req->stime ;
    //add qwj
    action = dw13 & 0xff;
    // Zone Reset 
    if (action == NVME_ZONE_ACTION_RESET){
        //reset zone->wp and zone->status=Empty
        for(int i=0;i<spp->nchips;i++)//add qwj
        {
            chip = &(n->zns->chips[i]);
            if(chip->next_avail_time > cmd_stime)
            {
                spp->reset_count++;
                printf("spp->reset_count = %ld\n",spp->reset_count);
            }
                
            nand_stime = (chip->next_avail_time < cmd_stime) ? cmd_stime : \
                     chip->next_avail_time;
            chip->next_avail_time = nand_stime + spp->blk_er_lat;
        }
        // spp->reset_count++;
        // if(spp->reset_count % 100 == 0)
        // printf("spp->reset_count = %ld\n",spp->reset_count);
        return ZONE_RESET_LATENCY;
    }
    // Read, Write 
    assert(opcode == NVME_CMD_WRITE || opcode == NVME_CMD_READ || opcode == NVME_CMD_ZONE_APPEND);
    if(req->is_write)
        return znsssd_write(n->zns, req);
    return znsssd_read(n->zns, req);
}


//zns定义的实际处理read命令的函数
static uint16_t zns_read(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                         NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb = (uint32_t)le16_to_cpu(rw->nlb) + 1;  //暂时不理解为什么要+1
    uint64_t data_size = zns_l2b(ns, nlb);
    uint64_t data_offset;
    uint16_t status;
#if SK_HYNIX_VALIDATION
    uint64_t nk = nlb/2;
    uint64_t delta_time = (uint64_t)nk*pow(10,9);   //n KB > 4096*1KB*2^10:10^9ns = 1KB : (10^9 / 2^10 / 4096)ns
    //femu_err("[Inho ] delt : %lx            ",delta_time);
    delta_time = delta_time/pow(2,10)/(Interface_PCIeGen3x4_bw);
    PCIe_Gen3_x4 * pcie = n->pci_simulation;
#endif
    assert(n->zoned);
    req->is_write = false;
    //检查max data transfer size
    status = nvme_check_mdts(n, data_size);
    if (status) {
        goto err;
    }
    //检查slba和nlb的边界条件
    status = zns_check_bounds(ns, slba, nlb);
    if (status) {
        goto err;
    }
    //检查zone的可读情况，包括可否跨zone读以及状态是否正常
    status = zns_check_zone_read(ns, slba, nlb);
    if (status) {
        goto err;
    }
    //完成prp到地址的映射
    status = zns_map_dptr(n, data_size, req);
    if (status) {
        goto err;
    }
    //如果err_rec中的dulbe为1,进一步执行dulbe检查
    if (NVME_ERR_REC_DULBE(n->features.err_rec)) {
        status = zns_check_dulbe(ns, slba, nlb);
        if (status) {
            goto err;
        }
    }
    //完成检查后计算字节为单位的数据起始偏移量
    data_offset = zns_l2b(ns, slba);
    /*模拟时延，并且直接在io路径上计算而不是直接在线程中计算，暂时取消
    req->expire_time += zns_advance_status(n,ns,cmd,req);
     */

    /*PCI 延迟模型*/
#if SK_HYNIX_VALIDATION
    //lock
    //pthread_spin_lock(&n->pci_lock);
    if(pcie->ntime + 2000 <  req->stime ){
        lag=0;
        pcie->stime = req->stime;
        pcie->ntime = pcie->stime + Interface_PCIeGen3x4_bwmb/NVME_DEFAULT_MAX_AZ_SIZE/1000 * delta_time;
    }else if(pcie->ntime < (pcie->stime + delta_time)){
        //update lag
        lag = (pcie->ntime - req->stime);
        pcie->stime = pcie->ntime;
        pcie->ntime = pcie->stime + Interface_PCIeGen3x4_bwmb/NVME_DEFAULT_MAX_AZ_SIZE/1000 * delta_time; //1ms
        req->expire_time += lag;
        pcie->stime += delta_time;
    }else if (req->stime < pcie->ntime && lag != 0 ){
        req->expire_time+=lag;
    }
    
    pcie->stime += delta_time;
    // femu_err("[inho] lag : %lx\n", lag);
    
    //pthread_spin_unlock(&n->pci_lock);
#endif
    //unlock
    //交付给后端memory进行实际的读取操作
    backend_rw(n->mbe, &req->qsg, &data_offset, req->is_write);
    return NVME_SUCCESS;

err:
    return status | NVME_DNR;
}
//zns定义的实际处理write命令的函数，与read可以说是基本一样
static uint16_t zns_write(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                          NvmeRequest *req)
{
    NvmeRwCmd *rw = (NvmeRwCmd *)cmd;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb = (uint32_t)le16_to_cpu(rw->nlb) + 1;
    uint64_t data_size = zns_l2b(ns, nlb);
    uint64_t data_offset;
    NvmeZone *zone;
    NvmeZonedResult *res = (NvmeZonedResult *)&req->cqe;
    uint16_t status;

    assert(n->zoned);
    req->is_write = true;
    status = nvme_check_mdts(n, data_size);
    if (status) {
        goto err;
    }

    status = zns_check_bounds(ns, slba, nlb);
    if (status) {
        goto err;
    }

    zone = zns_get_zone_by_slba(ns, slba);

    status = zns_check_zone_write(n, ns, zone, slba, nlb, false);
    if (status) {
        goto err;
    }
    status = zns_auto_open_zone(ns, zone);  //自动打开zone
    if (status) {
        goto err;
    }

    res->slba = zns_advance_zone_wp(ns, zone, nlb);     //取得写入完成后wp的值作为完成结果，也就是完成队列entry的slba
    data_offset = zns_l2b(ns, slba);                //取得偏移量
    status = zns_map_dptr(n, data_size, req);       //取得地址映射
    if (status) {
        goto err;
    }
    /*在io路径上推进时延，暂时取消
    req->expire_time += zns_advance_status(n,ns,cmd,req);
     */
    backend_rw(n->mbe, &req->qsg, &data_offset, req->is_write);     //通过后端实际写入数据
    zns_finalize_zoned_write(ns, req, false);                       //完成zone write的收尾工作

    return NVME_SUCCESS;

err:
    femu_err("*********ZONE WRITE FAILED*********, STATUS : %x\n",status);  
    return status | NVME_DNR;
}

//zns注册的io command操作
static uint16_t zns_io_cmd(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd,
                           NvmeRequest *req)
{

    switch (cmd->opcode) {
    case NVME_CMD_READ:
        return zns_read(n, ns, cmd, req);
    case NVME_CMD_WRITE:
        return zns_write(n, ns, cmd, req);
    case NVME_CMD_ZONE_MGMT_SEND:
        return zns_zone_mgmt_send(n, req);
    case NVME_CMD_ZONE_MGMT_RECV:
        return zns_zone_mgmt_recv(n, req);
    case NVME_CMD_ZONE_APPEND:
        return zns_zone_append(n, req);
    }

    return NVME_INVALID_OPCODE | NVME_DNR;
}

//设置序列号等信息
static void zns_set_ctrl_str(FemuCtrl *n)
{
    static int fsid_zns = 0;
    const char *zns_mn = "FEMU ZNS-SSD Controller"; //inhoinno: if i make another dev, rename this one
    const char *zns_sn = "vZNS-SSD"; //virtual ZNSSSd

    nvme_set_ctrl_name(n, zns_mn, zns_sn, &fsid_zns);
}

//设置控制，配置块
static void zns_set_ctrl(FemuCtrl *n)
{
    uint8_t *pci_conf = n->parent_obj.config;

    zns_set_ctrl_str(n);
    pci_config_set_vendor_id(pci_conf, PCI_VENDOR_ID_INTEL);
    pci_config_set_device_id(pci_conf, 0x5845);
}

//设置 zone相关参数能力
static int zns_init_zone_cap(FemuCtrl *n)
{
    n->zoned = true;
    n->zasl_bs = NVME_DEFAULT_MAX_AZ_SIZE;
    n->zone_size_bs = NVME_DEFAULT_ZONE_SIZE;
    n->zone_cap_bs = 0;
    n->cross_zone_read = false;
    n->max_active_zones = 0;
    n->max_open_zones = 0;
    n->zd_extension_size = 0;

    return 0;
}

static int zns_start_ctrl(FemuCtrl *n)
{
    /* Coperd: let's fail early before anything crazy happens */
    assert(n->page_size == 4096);

    if (!n->zasl_bs) {
        n->zasl = n->mdts;
    } else {
        if (n->zasl_bs < n->page_size) {
            femu_err("ZASL too small (%dB), must >= 1 page (4K)\n", n->zasl_bs);
            return -1;
        }
        n->zasl = 31 - clz32(n->zasl_bs / n->page_size);
    }

    return 0;
}
static int get_log2_base(int input){
    if(input==0){
        return 0;
    }
    int base=0;
    while(input!=1){
        input/=2;
        base++;
    }
    return base;
}
static void znsssd_init_params(FemuCtrl * n, struct zns_ssdparams *spp){

    //常规ssd参数设置
    spp->secsz = 512;
    spp->secs_per_pg = 8;   //page size : 4KB
    spp->pgs_per_blk = 256; //block size : 1MB
    spp->blks_per_pl = 32; //plane size = 32MB
    spp->pls_per_lun = 16;   //lun size = 512MB
    spp->luns_per_ch = 2;   //2way      >> 1 Inhoinno
    spp->nchs = 16;         //ssd, 16GB

    spp->pg_rd_lat = NAND_READ_LATENCY;
    spp->pg_wr_lat = NAND_PROG_LATENCY;
    spp->blk_er_lat = NAND_ERASE_LATENCY;
    spp->ch_xfer_lat = 0;

    /* calculated values */
    spp->secs_per_blk = spp->secs_per_pg * spp->pgs_per_blk;
    spp->secs_per_pl = spp->secs_per_blk * spp->blks_per_pl;
    spp->secs_per_lun = spp->secs_per_pl * spp->pls_per_lun;
    spp->secs_per_ch = spp->secs_per_lun * spp->luns_per_ch;
    spp->tt_secs = spp->secs_per_ch * spp->nchs;

    spp->pgs_per_pl = spp->pgs_per_blk * spp->blks_per_pl;
    spp->pgs_per_lun = spp->pgs_per_pl * spp->pls_per_lun;
    spp->pgs_per_ch = spp->pgs_per_lun * spp->luns_per_ch;
    spp->tt_pgs = spp->pgs_per_ch * spp->nchs;

    spp->blks_per_lun = spp->blks_per_pl * spp->pls_per_lun;
    spp->blks_per_ch = spp->blks_per_lun * spp->luns_per_ch;
    spp->tt_blks = spp->blks_per_ch * spp->nchs;

    spp->pls_per_ch =  spp->pls_per_lun * spp->luns_per_ch;
    spp->tt_pls = spp->pls_per_ch * spp->nchs;

    spp->tt_luns = spp->luns_per_ch * spp->nchs;





    //其他参数
    spp->pg_rd_lat = NAND_READ_LATENCY;
    spp->pg_wr_lat = NAND_PROG_LATENCY;
    spp->blk_er_lat = NAND_ERASE_LATENCY;
    spp->ch_xfer_lat = NAND_CHNL_PAGE_TRANSFER_LATENCY;
    spp->reset_count = 0;
    /**
     * 此处明确的体现了所谓的zone对多个channel的特点，假设说起初有nchnls为8，
     * 那么chnls_per_zone也刻意地保持了为8,同时删除ways
    */
    spp->nchnls         = 8;           /* FIXME : = ZNS_MAX_CHANNEL channel configuration like this */
    spp->zones          = n->num_zones;
    spp->chnls_per_zone = 8;

    /* TO REAL STORAGE SIZE */
    spp->csze_pages     = (((int64_t)n->memsz) * 1024 * 1024) / MIN_DISCARD_GRANULARITY / spp->nchnls / spp->ways;
    spp->nchips         = (((int64_t)n->memsz) * 1024 * 1024) / MIN_DISCARD_GRANULARITY / spp->csze_pages;//ways * nchnls



    //对数参数取对数
    spp->nchnls_log2 = get_log2_base(spp->nchnls);
    spp->secs_per_pg_log2 = get_log2_base(spp->secs_per_pg);
    spp->luns_per_ch_log2 = get_log2_base(spp->luns_per_ch);
    spp->pgs_per_blk_log2 = get_log2_base(spp->pgs_per_blk);
    spp->pgs_per_pl_log2 = get_log2_base(spp->pgs_per_pl);
    spp->pls_per_lun_log2 = get_log2_base(spp->pls_per_lun);
}

/**
 * @brief
 * @Inhoinno: we need to make zns ssd latency emulation
 * in order to emulate controller-level mapping in ZNS
 * for example, 1-to-1 mapping or 1-to-All mapping (zone-channel)
 * @param FemuCtrl for mapping channel for zones
 * @return none
 */
static void zns_init_ch(struct zns_ssd_channel *ch, struct zns_ssdparams *spp)
{
    //ch->nzones = spp->chnls_per_zone;
    /* ch->lun = g_malloc0(sizeof(struct nand_lun) * ch->nluns);
    for (int i = 0; i < ch->nluns; i++) {
        ssd_init_nand_lun(&ch->lun[i], spp);
    }*/
    ch->next_ch_avail_time = 0;
    ch->busy = 0;
}
static void zns_init_chip(struct zns_ssd_lun *chip, struct zns_ssdparams *spp)
{
    chip->next_avail_time = 0;
    chip->busy = 0;
}

void znsssd_init(FemuCtrl * n){
    struct zns *zns = n->zns = g_malloc0(sizeof(struct zns));
    struct zns_ssdparams *spp = &zns->sp;
    zns->namespaces = n->namespaces;
    znsssd_init_params(n, spp);
    /* initialize zns zone_tables映射 */
    zns->zone_tables = g_malloc0(sizeof (uint64_t) * n->num_zones);
    //这种计算法仍然考虑到了扇区
    uint64_t base =0;
    for(int i = 0; i< n->num_zones; i++){
        zns->zone_tables[i]=base;
        base+=n->zone_size;
    }
    /* initialize zns ssd internal layout architecture */
    zns->ch     = g_malloc0(sizeof(struct zns_ssd_channel) * spp->nchnls);
    zns->chips  = g_malloc0(sizeof(struct zns_ssd_lun) * spp->nchips);
    zns->zone_array = n->zone_array;
    zns->num_zones = spp->zones;

    for (int i = 0; i < spp->nchnls; i++) {
        zns_init_ch(&zns->ch[i], spp);
    }
    for (int i = 0; i < spp->nchips; i++) {
        zns_init_chip(&zns->chips[i], spp);
    }
}
//原生的zns-SSD实现，似乎只有一个namespace
static void zns_init(FemuCtrl *n, Error **errp)
{
    NvmeNamespace *ns = &n->namespaces[0];
    zns_set_ctrl(n);
    zns_init_zone_cap(n);
    if (zns_init_zone_geometry(ns, errp) != 0) {
        return;
    }

    zns_init_zone_identify(n, ns, 0);
    znsssd_init(n);         //物理时延模拟实现
}


static void zns_exit(FemuCtrl *n)
{
    /*
     * Release any extra resource (zones) allocated for ZNS mode
     */
}


/**
 * @brief
 * 1-to-1 model 一个zone只处于一个channel
 * zsze = 72M
 * transmit size=4K
 * 16chnl 2way
 * 获取ssd 物理页地址
 */


//由slba首先获取到ppa结构然后转换为ppn,注意每次使用完ppa都需要释放掉内存空间
static inline struct ppa zns_get_ppa(struct zns *zns, uint64_t slba){
    struct ppa PPA;
    struct zns_ssdparams sp=zns->sp;
    PPA.g.sec=(slba%sp.secs_per_pg)&0xFF; slba>>=sp.secs_per_pg_log2;
    PPA.g.ch=(slba%sp.nchnls)&0xFF;slba>>=sp.nchnls_log2;
    PPA.g.lun=(slba%sp.luns_per_ch&0xFF);slba>>=sp.luns_per_ch_log2;
    PPA.g.pl=(slba%sp.pls_per_lun&0xFF);slba>>=sp.pls_per_lun_log2;
    PPA.g.pg=(slba%sp.pgs_per_blk&0xFFFF);slba>>=sp.pgs_per_blk_log2;
    PPA.g.blk=(slba&0x7FFF);
    return PPA;
}
//注意此处的ppn与ppa中的值的区别，ppa描述的是ppa地址，但是需要能够快速映射到数组结构的方式从而方便的计算延时
static inline uint64_t zns_get_pg_idx(NvmeNamespace *ns,uint64_t slba){
    FemuCtrl  *n=ns->ctrl;
    struct zns_ssdparams *spp=n->zns->sp;
    struct ppa PPA= zns_get_ppa(n->zns,slba);
    uint64_t pgidx=PPA.g.ch*spp->pgs_per_ch +
            PPA.g.lun*spp->pgs_per_lun +
            PPA.g.pl*spp->pgs_per_pl +
            PPA.g.blk * spp->pgs_per_blk +
            PPA.g.pg;
    return pgidx;
}
//根据slba来获取到ppa从而可以判定对应地page所在的channel
static inline uint64_t zns_get_chnl_idx(NvmeNamespace *ns, uint64_t slba){
    FemuCtrl  *n=ns->ctrl;
    struct zns_ssdparams *spp=n->zns->sp;
    struct ppa PPA= zns_get_ppa(n->zns,slba);
    return PPA.g.ch;
}
//计算相对lun id
static inline uint64_t zns_get_lun_idx(NvmeNamespace *ns, uint64_t slba){
    FemuCtrl  *n=ns->ctrl;
    struct zns_ssdparams *spp=n->zns->sp;
    struct ppa PPA= zns_get_ppa(n->zns,slba);
    uint64_t lun_idx=PPA.g.ch*spp->luns_per_ch +
            PPA.g.lun;
    return lun_idx;
}

//计算相对plane id
static inline uint64_t zns_get_plane_idx(NvmeNamespace *ns, uint64_t slba){
    FemuCtrl  *n=ns->ctrl;
    struct zns_ssdparams *spp=n->zns->sp;
    struct ppa PPA= zns_get_ppa(n->zns,slba);
    uint64_t pl_idx=PPA.g.ch*spp->pls_per_ch +
                 PPA.g.lun*spp->pls_per_lun +
                 PPA.g.pl;
    return pl_idx;
}

//计算相对block id
static inline uint64_t zns_get_block_idx(NvmeNamespace *ns, uint64_t slba){

    FemuCtrl  *n=ns->ctrl;
    struct zns_ssdparams *spp=n->zns->sp;
    struct ppa PPA= zns_get_ppa(n->zns,slba);
    uint64_t blk_idx=PPA.g.ch*spp->blks_per_ch +
                PPA.g.lun*spp->blks_per_lun +
                PPA.g.pl*spp->blks_per_pl+
                PPA.g.blk;
#ifdef debug_mode
    uint64_t zoneidx=zns_zone_idx(ns,slba);
    assert(block_id==zoneidx);
#endif
    return blk_idx;

}

//定制写操作延时
static uint64_t znsssd_write(ZNS *zns, NvmeRequest *req){
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    struct NvmeNamespace *ns = req->ns;
    struct zns_ssdparams * spp = &zns->sp;
    uint64_t slba = le64_to_cpu(rw->slba);
    uint32_t nlb = (uint32_t)le16_to_cpu(rw->nlb) + 1;
    zns_ssd_lun *chip = NULL;
    uint64_t currlat = 0, maxlat= 0;
    uint32_t my_chip_idx = 0;
    uint64_t nand_stime =0;
    uint64_t cmd_stime = (req->stime == 0) ? qemu_clock_get_ns(QEMU_CLOCK_REALTIME) : req->stime ;
#if ADVANCE_PER_CH_ENDTIME
    zns_ssd_channel *chnl =NULL;
    uint32_t my_chnl_idx = 0;
    uint64_t chnl_stime =0;
#endif


    for (uint32_t i = 0; i<nlb ; i+=32){//mod qwj
        //Inhoinno : Interleaving per 16KB
        slba += i;
#if SK_HYNIX_VALIDATION
        my_chip_idx=hynix_zns_get_lun_idx(ns,slba); //SK Hynix
#endif
#if !(SK_HYNIX_VALIDATION)
        my_chip_idx=zns_get_multiway_chip_idx(ns, slba);
#endif
        chip = &(zns->chips[my_chip_idx]);
#if !(ADVANCE_PER_CH_ENDTIME)
        //Inhoinno:  Single thread emulation so assume we dont need lock per chnl
        nand_stime = (chip->next_avail_time < cmd_stime) ? cmd_stime : \
                     chip->next_avail_time;
        chip->next_avail_time = nand_stime + spp->pg_wr_lat;
        currlat= chip->next_avail_time - cmd_stime ; //Inhoinno : = T_channel + T_chip(=chnl->next_available_time) - stime; // FIXME like this
        maxlat = (maxlat < currlat)? currlat : maxlat;
#endif
#if ADVANCE_PER_CH_ENDTIME
#if SK_HYNIX_VALIDATION
        my_chnl_idx = hynix_zns_get_chnl_idx(ns, slba); //SK Hynix
#endif
#if !(SK_HYNIX_VALIDATION)
        my_chnl_idx=zns_advanced_chnl_idx(ns, slba);
#endif
        chnl = &(zns->ch[my_chnl_idx]);
        chnl_stime = (chnl->next_ch_avail_time < cmd_stime) ? cmd_stime : \
                     chnl->next_ch_avail_time;
        chnl->next_ch_avail_time = chnl_stime + spp->ch_xfer_lat;

        // write: then do NAND program
        nand_stime = (chip->next_avail_time < chnl->next_ch_avail_time) ? \
            chnl->next_ch_avail_time : chip->next_avail_time;
        chip->next_avail_time = nand_stime + spp->pg_wr_lat;
        currlat = chip->next_avail_time - cmd_stime;
        maxlat = (maxlat < currlat)? currlat : maxlat;
#endif

    }
    return maxlat;

}
//定制读操作延时
static uint64_t znsssd_read(ZNS *zns, NvmeRequest *req){
    NvmeRwCmd *rw = (NvmeRwCmd *)&req->cmd;
    uint64_t slba = le64_to_cpu(rw->slba);//以512B扇区为单位
    uint32_t nlb = (uint32_t)le16_to_cpu(rw->nlb) + 1;
    struct NvmeNamespace *ns = req->ns;
    struct zns_ssdparams * spp = &zns->sp;
    zns_ssd_lun *chip = NULL;
    uint64_t currlat = 0, maxlat= 0;
    uint32_t my_chip_idx = 0;
    uint64_t nand_stime =0;
    uint64_t cmd_stime = (req->stime == 0) ? qemu_clock_get_ns(QEMU_CLOCK_REALTIME) : req->stime ;
#if ADVANCE_PER_CH_ENDTIME
    zns_ssd_channel *chnl =NULL;
    uint32_t my_chnl_idx = 0;
    uint64_t chnl_stime =0;
#endif

    for (uint64_t i = 0; i<nlb ; i+=32){//mod qwj
        //Inhoinno : Interleaving per 16KB
        slba += i;
#if SK_HYNIX_VALIDATION
        my_chip_idx=hynix_zns_get_lun_idx(ns,slba); //SK Hynix
#endif
#if !(SK_HYNIX_VALIDATION)
        my_chip_idx=zns_get_multiway_chip_idx(ns, slba);
#endif
        chip = &(zns->chips[my_chip_idx]);
        //Inhoinno:  Single thread emulation so assume we dont need lock per chnl
        if(chip->next_avail_time > cmd_stime)
        {

        }
        else{

        }

        nand_stime = (chip->next_avail_time < cmd_stime) ? cmd_stime : \
                     chip->next_avail_time;
#if !(ADVANCE_PER_CH_ENDTIME)

        chip->next_avail_time = nand_stime + spp->pg_rd_lat;
        currlat= chip->next_avail_time - cmd_stime ; //Inhoinno : = T_channel + T_chip(=chnl->next_available_time) - stime; // FIXME like this
        maxlat = (maxlat < currlat)? currlat : maxlat;
#endif
#if ADVANCE_PER_CH_ENDTIME
#if SK_HYNIX_VALIDATION
        my_chnl_idx = hynix_zns_get_chnl_idx(ns, slba); //SK Hynix
#endif
#if !(SK_HYNIX_VALIDATION)
        my_chnl_idx=zns_advanced_chnl_idx(ns, slba);
#endif

        chnl = &(zns->ch[my_chnl_idx]);

        chip->next_avail_time = nand_stime + spp->pg_rd_lat;

        //read: then data transfer through channel
        chnl_stime = (chnl->next_ch_avail_time < chip->next_avail_time) ? \
            chip->next_avail_time : chnl->next_ch_avail_time;
        chnl->next_ch_avail_time = chnl_stime + spp->ch_xfer_lat;

        currlat = chnl->next_ch_avail_time - cmd_stime;
        maxlat = (maxlat < currlat)? currlat : maxlat;
#endif

    }
    return maxlat;
}


/*
 * 类似于ftl_thread的zns线程处理，用于提供物理时延模拟
 * 作为一个qemu thread的执行函数存在。
 * 对于每个请求会更新req->time
 */
static void *zns_thread(void *arg){
    FemuCtrl *n = (FemuCtrl *)arg;
    struct zns *zns = n->zns;
    NvmeRequest *req = NULL;
    uint64_t lat = 0;
    int rc=1;
    int i;

    femu_err(" zns_thread starts running\n");
    while (!*(zns->dataplane_started_ptr)) {
        usleep(100000);
    }
    // FIXME: not safe, to handle ->to_ftl and ->to_poller gracefully
    zns->to_zone = n->to_ftl;
    zns->to_poller = n->to_poller;

    while (1) {
        for (i = 1; i <= n->num_poller; i++) {
            if (!zns->to_zone[i] || !femu_ring_count(zns->to_zone[i]))
                continue;
            //ISSUE : this is problem?
            rc = femu_ring_dequeue(zns->to_zone[i], (void *)&req, 1);
            if (rc != 1) {
                femu_err("FEMU: ZNS_thread to_zone dequeue failed\n");
            }

            //ftl_assert(req);
            switch (req->cmd.opcode) {
                case NVME_CMD_WRITE:
                    lat = znsssd_write(zns, req);
                    break;
                case NVME_CMD_READ:
                    lat = znsssd_read(zns, req);
                    break;
                case NVME_CMD_DSM:
                    lat = 0;
                    break;
                default:
                    femu_err("ZNS SSD received unkown request type, ERROR\n");
            }

            req->reqlat = lat;
            req->expire_time += lat;

            rc = femu_ring_enqueue(zns->to_poller[i], (void *)&req, 1);
            if (rc != 1) {
                femu_err("ZNS_thread to_poller enqueue failed\n");
            }

            // no gc in zns, only reset zone
            //TODO: Copy-back op
        }

    }

    return NULL;
}


int nvme_register_dynamic_znsssd(FemuCtrl *n)
{

    femu_err("dynamic-zns/zns.c : nvme_register_dynamic_znsssd()\n");
    n->ext_ops = (FemuExtCtrlOps) {
            .state            = NULL,
            .init             = zns_init,
            .exit             = zns_exit,
            .rw_check_req     = NULL,
            .start_ctrl       = zns_start_ctrl,
            .admin_cmd        = zns_admin_cmd,
            .io_cmd           = zns_io_cmd,
            .get_log          = NULL,
    };
    return 0;
}


