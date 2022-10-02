#ifndef __FEMU_FTL_H
#define __FEMU_FTL_H

#include "../nvme.h"

#define INVALID_PPA     (~(0ULL))
#define INVALID_LPN     (~(0ULL))
#define UNMAPPED_PPA    (~(0ULL))

//基础NAND操作相关的参数
enum {
    NAND_READ =  0,
    NAND_WRITE = 1,
    NAND_ERASE = 2,

    NAND_READ_LATENCY = 40000,
    NAND_PROG_LATENCY = 500000,
    NAND_ERASE_LATENCY= 2000000,
};

//区分用户IO和GC IO
enum {
    USER_IO = 0,
    GC_IO = 1,
};


//page和section（？)的状态
enum {
    SEC_FREE = 0,
    SEC_INVALID = 1,
    SEC_VALID = 2,

    PG_FREE = 0,
    PG_INVALID = 1,
    PG_VALID = 2
};
//是否允许GC延迟，是否允许延迟模拟
enum {
    FEMU_ENABLE_GC_DELAY = 1,
    FEMU_DISABLE_GC_DELAY = 2,

    FEMU_ENABLE_DELAY_EMU = 3,
    FEMU_DISABLE_DELAY_EMU = 4,

    FEMU_RESET_ACCT = 5,
    FEMU_ENABLE_LOG = 6,
    FEMU_DISABLE_LOG = 7,
};


//当前模式下把64位的物理页地址进行如下的映射
#define BLK_BITS    (16)
#define PG_BITS     (16)
#define SEC_BITS    (8)
#define PL_BITS     (8)
#define LUN_BITS    (8)
#define CH_BITS     (7)

/*用于描述一个物理页地址*/
struct ppa {
    union {
        struct {
            uint64_t blk : BLK_BITS;
            uint64_t pg  : PG_BITS;
            uint64_t sec : SEC_BITS;
            uint64_t pl  : PL_BITS;
            uint64_t lun : LUN_BITS;
            uint64_t ch  : CH_BITS;
            uint64_t rsv : 1;
        } g;

        uint64_t ppa;
    };
};


typedef int nand_sec_status_t;


//用于描述访问到nand page的信息
struct nand_page {
    nand_sec_status_t *sec;
    int nsecs;
    int status;
};

//用于描述nand block的信息
struct nand_block {
    struct nand_page *pg; //应该是page数组
    int npgs;   //page页数
    int ipc; /* invalid page count */
    int vpc; /* valid page count */
    int erase_cnt;  //擦除次数
    int wp; /* current write pointer */
};
//用于描述nand plane结构
struct nand_plane {
    struct nand_block *blk; //block数组
    int nblks;  //block个数
};
//用于描述 nand logical unit (die，也可能作为chip的抽象)
struct nand_lun {
    struct nand_plane *pl;      //plane数组
    int npls;       // plane个数
    uint64_t next_lun_avail_time;   //die作为可以产生并行性的单位，需要记录下一次的可访问时间
    bool busy;      //当前是否繁忙
    uint64_t gc_endtime;        //gc结束时间
};
//用于描述ssd内部的channel
struct ssd_channel {
    struct nand_lun *lun;       //logical unit数组
    int nluns;  //logical unit个数
    uint64_t next_ch_avail_time;    //记录下一次的可访问时间
    bool busy;  //当前是否繁忙
    uint64_t gc_endtime;    //gc结束时间
};


//与一个黑盒ssd相关的参数
struct ssdparams {
    //基本参数
    int secsz;        /* sector size in bytes */
    int secs_per_pg;  /* # of sectors per page */
    int pgs_per_blk;  /* # of NAND pages per block */
    int blks_per_pl;  /* # of blocks per plane */
    int pls_per_lun;  /* # of planes per LUN (Die) */
    int luns_per_ch;  /* # of LUNs per channel */
    int nchs;         /* # of channels in the SSD */
    //延迟信息
    int pg_rd_lat;    /* NAND page read latency in nanoseconds */
    int pg_wr_lat;    /* NAND page program latency in nanoseconds */
    int blk_er_lat;   /* NAND block erase latency in nanoseconds */
    int ch_xfer_lat;  /* channel transfer latency for one page in nanoseconds
                       * this defines the channel bandwith
                       */
    //gc 参数
    double gc_thres_pcent;  //触发gc的page门限值？
    int gc_thres_lines;     //触发gc的line门限值？
    double gc_thres_pcent_high;     //gc page门限值高水位？
    int gc_thres_lines_high;        //gc line门限值高水位？
    bool enable_gc_delay;           //是否开启gc延迟

    /* below are all calculated values;通过计算可以获得的值*/
    int secs_per_blk; /* # of sectors per block */
    int secs_per_pl;  /* # of sectors per plane */
    int secs_per_lun; /* # of sectors per LUN */
    int secs_per_ch;  /* # of sectors per channel */
    int tt_secs;      /* # of sectors in the SSD */

    int pgs_per_pl;   /* # of pages per plane */
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
};

//ssd中的line概念?
typedef struct line {
    int id;  /* line id, the same as corresponding block id */
    int ipc; /* invalid page count in this line */
    int vpc; /* valid page count in this line */
    QTAILQ_ENTRY(line) entry; /* in either {free,victim,full} list */
    /* position in the priority queue for victim lines */
    size_t                  pos;
} line;



/* wp: record next write addr,记录了下一个可写的地址信息*/
struct write_pointer {
    struct line *curline;   //当前的line
    //其他的单位号？
    int ch;
    int lun;
    int pg;
    int blk;
    int pl;
};

struct line_mgmt {
    struct line *lines;
    /* free line list, we only need to maintain a list of blk numbers */
    QTAILQ_HEAD(free_line_list, line) free_line_list;
    pqueue_t *victim_line_pq;
    //QTAILQ_HEAD(victim_line_list, line) victim_line_list;
    QTAILQ_HEAD(full_line_list, line) full_line_list;
    int tt_lines;
    int free_line_cnt;
    int victim_line_cnt;
    int full_line_cnt;
};


//nand 指令
struct nand_cmd {
    int type;       //可以区分是USER IO还是GC IO
    int cmd;        //可以取值NAND_READ，NAND_WRITE,NAND_ERASE等
    int64_t stime; /* Coperd: request arrival time */
};

//黑盒SSD结构
struct ssd {
    char *ssdname;
    struct ssdparams sp;
    struct ssd_channel *ch;
    struct ppa *maptbl; /* page level mapping table;ppa结构数组，从下标定位到ppa？*/
    uint64_t *rmap;     /* reverse mapptbl, assume it's stored in OOB */
    struct write_pointer wp;
    struct line_mgmt lm;    //line管理？

    /* lockless ring for communication with NVMe IO thread */
    struct rte_ring **to_ftl;
    struct rte_ring **to_poller;
    bool *dataplane_started_ptr;
    QemuThread ftl_thread;
};

void ssd_init(FemuCtrl *n);

#ifdef FEMU_DEBUG_FTL
#define ftl_debug(fmt, ...) \
    do { printf("[FEMU] FTL-Dbg: " fmt, ## __VA_ARGS__); } while (0)
#else
#define ftl_debug(fmt, ...) \
    do { } while (0)
#endif

#define ftl_err(fmt, ...) \
    do { fprintf(stderr, "[FEMU] FTL-Err: " fmt, ## __VA_ARGS__); } while (0)

#define ftl_log(fmt, ...) \
    do { printf("[FEMU] FTL-Log: " fmt, ## __VA_ARGS__); } while (0)


/* FEMU assert() */
#ifdef FEMU_DEBUG_FTL
#define ftl_assert(expression) assert(expression)
#else
#define ftl_assert(expression)
#endif

#endif
