#ifndef __FEMU_ZNS_H
#define __FEMU_ZNS_H

#include "../nvme.h"

enum {
    NAND_READ =  0,
    NAND_WRITE = 1,
    NAND_ERASE = 2,

 /* TLC_ QWJ */
    NAND_READ_LATENCY  = TLC_CENTER_PAGE_READ_LATENCY_NS,  
    NAND_PROG_LATENCY  = TLC_CENTER_PAGE_WRITE_LATENCY_NS,
    NAND_ERASE_LATENCY = TLC_BLOCK_ERASE_LATENCY_NS,
    NAND_CHNL_PAGE_TRANSFER_LATENCY = TLC_CHNL_PAGE_TRANSFER_LATENCY_NS, 
    
    ZONE_RESET_LATENCY =  TLC_BLOCK_ERASE_LATENCY_NS / 10,

};

//LBA映射到zone，根据zone的状态取得对应写入的ppa

//当前模式下把64位的物理页地址进行如下的映射
#define PG_BITS     (16)
#define BLK_BITS    (15)
#define SEC_BITS    (8)
#define PL_BITS     (8)
#define LUN_BITS    (8)
#define CH_BITS     (8)

/*用于描述一个物理页地址，随着物理页地址的变化，反而是channel先增加，符合channel的改变*/
struct ppa {
    union {
        struct {
            uint64_t sec : SEC_BITS;
            uint64_t ch  : CH_BITS;
            uint64_t lun : LUN_BITS;
            uint64_t pl  : PL_BITS;
            uint64_t pg  : PG_BITS;
            uint64_t blk : BLK_BITS;
            uint64_t rsv :  1;
        } g;
        uint64_t ppa;
    };
};
typedef int nand_sec_status_t;
/*
 * 暂时未确定哪些字段需要使用
 */
struct zns_nand_page {
    nand_sec_status_t *sec;
    int nsecs;
    int status;
};

//用于描述nand block的信息
typedef struct zns_nand_block {
    struct nand_page *pg; //应该是page数组
    int npgs;   //page页数
    int ipc; /* invalid page count */
    int vpc; /* valid page count */
    int erase_cnt;  //擦除次数
    int wp; /* current write pointer */
}zns_nand_block;

//用于描述nand plane结构
typedef struct zns_nand_plane {
    zns_nand_block *blk; //block数组
    int nblks;  //block个数
}zns_nand_plane;

/*
 * 实现lun的结构，单纯用于时延模拟的相关结构
 */
typedef struct zns_ssd_lun {
    zns_nand_plane *planes;
    int npls;
    uint64_t next_avail_time; // in nanoseconds
    bool busy;

}zns_ssd_lun;

/*
 * 实现zone在ssd内部的mapping
 * 单纯用于时延模拟的相关结构
 */
typedef struct zns_ssd_channel {
    int nzones;     //每个channel内的zone的个数,在后续修改为可调整zone后其实仍然是不变的
    zns_ssd_lun *luns;
    int nluns;
    uint64_t next_ch_avail_time; 
    bool busy;
}zns_ssd_channel;

typedef struct zone_table_entry {
    uint16_t chnl_map;
    uint64_t page_start;
    uint8_t block_size;
}zone_table_entry;
/*
 * @brief 
 * 与延迟模拟相关的zns-ssd 参数
 */
struct zns_ssdparams{
    /* basic params*/
    int secsz;        /* sector size in bytes */
    int secs_per_pg;  /* # of sectors per page */
    int secs_per_pg_log2;
    int pgs_per_blk;  /* # of NAND pages per block */
    int pgs_per_blk_log2;
    int blks_per_pl;  /* # of blocks per plane */
    int pls_per_lun;  /* # of planes per LUN (Die) */
    int pls_per_lun_log2;
    int luns_per_ch;  /* # of LUNs per channel */
    int luns_per_ch_log2;  /* # of LUNs per channel */

    int secs_per_blk; /* # of sectors per block */
    int secs_per_pl;  /* # of sectors per plane */
    int secs_per_lun; /* # of sectors per LUN */
    int secs_per_ch;  /* # of sectors per channel */
    int tt_secs;      /* # of sectors in the SSD */

    /* calculated values*/
    int pgs_per_pl;   /* # of pages per plane */
    int pgs_per_pl_log2;
    int pgs_per_lun;  /* # of pages per LUN (Die) */
    int pgs_per_ch;   /* # of pages per channel */
    int tt_pgs;       /* total # of pages in the SSD */

    int blks_per_lun; /* # of blocks per LUN */
    int blks_per_ch;  /* # of blocks per channel */
    int tt_blks;      /* total # of blocks in the SSD */

    int secs_per_line;
    int pgs_per_line;
    int blks_per_line;
    int tt_lines;

    int pls_per_ch;   /* # of planes per channel */
    int tt_pls;       /* total # of planes in the SSD */

    int tt_luns;      /* total # of LUNs in the SSD */

    /*zone info*/
    uint16_t register_model;    /* =1 single register =2 double register */
    uint64_t nchnls;            /* # of channels in the SSD */
    uint64_t nchnls_log2;
    uint64_t zones;             /* # of zones in ZNS SSD */
    uint64_t chnls_per_zone;    /* ZNS Association degree. # of zones per channel?, must be divisor of nchnls */
    uint64_t csze_pages;        /* #of Pages in Chip (Inhoinno:I guess lun in femu)*/
    uint64_t nchips;            /* # of chips in SSD*/

    uint64_t pg_rd_lat;         /* NAND page read latency in nanoseconds */
    uint64_t pg_wr_lat;         /* NAND page program latency in nanoseconds */
    uint64_t blk_er_lat;        /* NAND block erase latency in nanoseconds */
    uint64_t zone_reset_lat;    /* ZNS SSD ZONE reset latency in nanoseconds */
    uint64_t ch_xfer_lat;       /* channel transfer latency for one page in nanoseconds*/
    uint64_t reset_count;
};

/*从ftl的ssd结构中继承了部分的共同属性，同时新增了ZNS-SSD专用的信息*/
typedef struct zns {
    /*members from struct ssd*/
    char                *ssdname;
    struct zns_ssdparams    sp;
    struct zns_ssd_channel *ch;
    struct zns_ssd_lun *chips;
    //zone级别的ppn映射表,目前为固定的格式
    zone_table_entry *zone_tables;

    /*new members for znsssd*/
    struct rte_ring **to_zone;
    struct rte_ring **to_poller;
    bool *dataplane_started_ptr;
    struct NvmeNamespace    * namespaces;      //FEMU only support 1 namespace For now,
    //struct NvmeZone      * zone_array; 定制的nvmezone? 但是目前直接使用FemuCtrl中的
    uint32_t            num_zones;
    QemuThread          zns_thread;


}ZNS;

typedef struct QEMU_PACKED NvmeZonedResult {
    uint64_t slba;
} NvmeZonedResult;

//zone 内的identify controller结构
//https://nvmexpress.org/wp-content/uploads/NVM-Zoned-Namespace-Command-Set-Specification-1.1b-2022.01.05-Ratified.pdf figure50
typedef struct NvmeIdCtrlZoned {
    uint8_t     zasl;           /*  [Zone Append Size Limit]如果此字段为非0值，代表着单次Zone Append最大的数据传输大小
                                 *  值为 2 的 zasl 次方乘以最小内存页面大小 (定义在CAP.MPSMIN 一般为4096 B)，且不能超过MDTS
                                 *  如果此字段为0，那么单次Zone Append最大的数据传输大小直接受nvme base标准中的MDTS字段来限制
                                 */
    uint8_t     rsvd1[4095];   //reserved : 
} NvmeIdCtrlZoned;

//配合NVmeZoneDescriptor za使用
enum NvmeZoneAttr {
    NVME_ZA_FINISHED_BY_CTLR         = 1 << 0,      //ZFC   设置为1时，controller通过一个Zone Active Excursion来完成一个zone
    NVME_ZA_FINISH_RECOMMENDED       = 1 << 1,      //FZR   设置为1时，controller推荐这个zone被设置为完成？
    NVME_ZA_RESET_RECOMMENDED        = 1 << 2,      //RZR   设置为1时，controller推荐这个zone被重置
    NVME_ZA_ZD_EXT_VALID             = 1 << 7,      //ZDEV  设置为1时，会有一个Zone Descriptor Extension Data与这个zone关联起来
};

typedef struct QEMU_PACKED NvmeZoneReportHeader {
    uint64_t    nr_zones;
    uint8_t     rsvd[56];
} NvmeZoneReportHeader;

enum NvmeZoneReceiveAction {
    NVME_ZONE_REPORT                 = 0,
    NVME_ZONE_REPORT_EXTENDED        = 1,
};

//NvmeZone 报告类型
enum NvmeZoneReportType {
    NVME_ZONE_REPORT_ALL             = 0,
    NVME_ZONE_REPORT_EMPTY           = 1,
    NVME_ZONE_REPORT_IMPLICITLY_OPEN = 2,
    NVME_ZONE_REPORT_EXPLICITLY_OPEN = 3,
    NVME_ZONE_REPORT_CLOSED          = 4,
    NVME_ZONE_REPORT_FULL            = 5,
    NVME_ZONE_REPORT_READ_ONLY       = 6,
    NVME_ZONE_REPORT_OFFLINE         = 7,
};

//nvme内zone的类型
enum NvmeZoneType {
    NVME_ZONE_TYPE_RESERVED          = 0x00,        //保留
    //for test, inhoinno
    NVME_ZONE_TYPE_CONVENTIONAL      = 0x01,
    NVME_ZONE_TYPE_SEQ_WRITE         = 0x02,        //顺序写
};

enum NvmeZoneSendAction {
    NVME_ZONE_ACTION_RSD             = 0x00,
    NVME_ZONE_ACTION_CLOSE           = 0x01,//Closing the zone releases the open resource and can be done on both explicitly open zones and implicitly open zones.
    NVME_ZONE_ACTION_FINISH          = 0x02,//Finishing a zone sets its state to 'full'.
    NVME_ZONE_ACTION_OPEN            = 0x03,//Explicitly opening a zone makes it ready for immediate write access and consumes an Open Resource. 
    NVME_ZONE_ACTION_RESET           = 0x04,//To reset the write pointer and return a zone to the EMPTY state, use the 'zone-reset' command.
    NVME_ZONE_ACTION_OFFLINE         = 0x05,//Offlining a zone makes the zone inaccessible. The data on the offlined zone will no longer be accessible, and writes to the zone will not be possible until the zone is reset.
    NVME_ZONE_ACTION_SET_ZD_EXT      = 0x10,
};
//NVMe的zone描述符
//https://nvmexpress.org/wp-content/uploads/NVM-Zoned-Namespace-Command-Set-Specification-1.1b-2022.01.05-Ratified.pdf figure37
typedef struct QEMU_PACKED NvmeZoneDescr {
    uint8_t     zt;         //[Zone Type] 取值NVME_ZONE_TYPE_SEQ_WRITE(does sequential wirte required? NVME TP4053a section 2.3.1)
    uint8_t     zs;         //[Zone State]初始化赋值时把实际enum元素移位到高位去了，这应该与规定的格式有关系
    uint8_t     za;         //[Zone Attributes]
    uint8_t     rsvd3[5];
    uint64_t    zcap;       //[zone capacity]n->zone_capacity
    uint64_t    zslba;      //[Zone Start Logical Block Address]注意这里的逻辑块地址是相对于namespace内的所有zone的全局地址
    uint64_t    wp;         //[Write pointer]
    uint8_t     rsvd32[32];
} NvmeZoneDescr;

//zone的几种状态与上面的zs相关
typedef enum NvmeZoneState {
    NVME_ZONE_STATE_RESERVED         = 0x00,
    NVME_ZONE_STATE_EMPTY            = 0x01,
    NVME_ZONE_STATE_IMPLICITLY_OPEN  = 0x02,
    NVME_ZONE_STATE_EXPLICITLY_OPEN  = 0x03,
    NVME_ZONE_STATE_CLOSED           = 0x04,
    NVME_ZONE_STATE_READ_ONLY        = 0x0D,
    NVME_ZONE_STATE_FULL             = 0x0E,
    NVME_ZONE_STATE_OFFLINE          = 0x0F,
} NvmeZoneState;

#define NVME_SET_CSI(vec, csi) (vec |= (uint8_t)(1 << (csi)))

//LBA Format Extension 16字节大小
typedef struct QEMU_PACKED NvmeLBAFE {
    uint64_t    zsze;       //[zone size]n->zone_size  Define the size of each zone in the namespaces,注意是zone内逻辑块的个数  [0x20000 LBAs]
    uint8_t     zdes;       //[Zone Descriptor Extension Size]n->zd_extension_size >> 6 ，注意本值的单位是64字节
    uint8_t     rsvd9[7];
} NvmeLBAFE;

//identify namespaces zoned命令返回结构
//https://nvmexpress.org/wp-content/uploads/NVMe-Zoned-Namespace-Command-Set-Specification-1.1a-2021.07.26-Ratified.pdf
typedef struct QEMU_PACKED NvmeIdNsZoned {
    uint16_t    zoc;            //[Zone Operation Characteristics]第0位代表zone可以在不改变namespace格式的情况下调整zone的capacity
    uint16_t    ozcs;           //[Optional Zoned Command Support]n->cross_zone_read ? 0x01 : 0x00;
    uint32_t    mar;            //[max active resources]n->max_active_zones ; 0xffffffff means no limit
    uint32_t    mor;            //[max open resources]n->max_open_zones ; 0xffffffff means no limit ;If zoned.max_active is specified, this value must be less than or equal to that.
    uint32_t    rrl;            //[reset recommended limit]
    uint32_t    frl;            //[finish recommended limit]
    uint8_t     rsvd20[2796];
    NvmeLBAFE   lbafe[16];      //[LBA format extension]
    uint8_t     rsvd3072[768];
    uint8_t     vs[256];        //[vendor specific]
} NvmeIdNsZoned;

//nvme的zone
typedef struct NvmeZone {
    NvmeZoneDescr   d;
    uint64_t        w_ptr;
    QTAILQ_ENTRY(NvmeZone) entry;
} NvmeZone;

typedef struct NvmeNamespaceParams {
    uint32_t nsid;
    QemuUUID uuid;

    bool     zoned;
    bool     cross_zone_read;
    uint64_t zone_size_bs;
    uint64_t zone_cap_bs;
    uint32_t max_active_zones;
    uint32_t max_open_zones;
    uint32_t zd_extension_size;     //Set the [Zone Descriptor Extension Size] (ZDES). Must be a multiple of 64 bytes.
} NvmeNamespaceParams;

static inline uint32_t zns_nsid(NvmeNamespace *ns)
{
    if (ns) {
        return ns->id;
    }

    return -1;
}

//获取lba格式
static inline NvmeLBAF *zns_ns_lbaf(NvmeNamespace *ns)
{
    NvmeIdNs *id_ns = &ns->id_ns;
    return &id_ns->lbaf[NVME_ID_NS_FLBAS_INDEX(id_ns->flbas)];
}
//获取lbads，也就是扇区大小
static inline uint8_t zns_ns_lbads(NvmeNamespace *ns)
{
    /* NvmeLBAF */
    return zns_ns_lbaf(ns)->lbads;
}

/* calculate the number of LBAs that the namespace can accomodate */
static inline uint64_t zns_ns_nlbas(NvmeNamespace *ns)
{
    return ns->size >> zns_ns_lbads(ns);
}

//将逻辑块的数量转换为bytes 数
static inline size_t zns_l2b(NvmeNamespace *ns, uint64_t lba)
{
    return lba << zns_ns_lbads(ns);
}
//获取zone的状态
static inline NvmeZoneState zns_get_zone_state(NvmeZone *zone)
{
    return zone->d.zs >> 4;
}

//设置zone的实际状态
static inline void zns_set_zone_state(NvmeZone *zone, NvmeZoneState state)
{
    zone->d.zs = state << 4;
}
//获取zone的read右侧边界lba 左闭右开
static inline uint64_t zns_zone_rd_boundary(NvmeNamespace *ns, NvmeZone *zone)
{
    return zone->d.zslba + ns->ctrl->zone_size;
}
//获取zone的write右侧边界lba 左闭右开
static inline uint64_t zns_zone_wr_boundary(NvmeZone *zone)
{
    return zone->d.zslba + zone->d.zcap;    //与read不同的是，由capacity来控制
}
//检查zns 写指针的状态，这里其实就是检查状态
static inline bool zns_wp_is_valid(NvmeZone *zone)
{
    uint8_t st = zns_get_zone_state(zone);

    return st != NVME_ZONE_STATE_FULL &&
           st != NVME_ZONE_STATE_READ_ONLY &&
           st != NVME_ZONE_STATE_OFFLINE;
}

static inline uint8_t *zns_get_zd_extension(NvmeNamespace *ns, uint32_t zone_idx)
{
    return &ns->ctrl->zd_extensions[zone_idx * ns->ctrl->zd_extension_size];
}
//active and open resouces 增加open数量
static inline void zns_aor_inc_open(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    assert(n->nr_open_zones >= 0);
    if (n->max_open_zones) {
        n->nr_open_zones++;
        assert(n->nr_open_zones <= n->max_open_zones);
    }
}
//active and open resouces 减少open数量
static inline void zns_aor_dec_open(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_open_zones) {
        assert(n->nr_open_zones > 0);
        n->nr_open_zones--;
    }
    assert(n->nr_open_zones >= 0);
}
//active and open resouces 增加active数量
static inline void zns_aor_inc_active(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    assert(n->nr_active_zones >= 0);
    if (n->max_active_zones) {
        n->nr_active_zones++;
        assert(n->nr_active_zones <= n->max_active_zones);
    }
}
//active and open resouces 减少active数量
static inline void zns_aor_dec_active(NvmeNamespace *ns)
{
    FemuCtrl *n = ns->ctrl;
    if (n->max_active_zones) {
        assert(n->nr_active_zones > 0);
        n->nr_active_zones--;
        assert(n->nr_active_zones >= n->nr_open_zones);
    }
    assert(n->nr_active_zones >= 0);
}

void zns_ns_shutdown(NvmeNamespace *ns);
void zns_ns_cleanup(NvmeNamespace *ns);


//get_zone(Namespace, req)
//get_ch(Zone, req)

void znsssd_init(FemuCtrl * n);
static int zns_advance_status(FemuCtrl *n, NvmeNamespace *ns, NvmeCmd *cmd, NvmeRequest *req);

#endif
