#ifndef __CLUSTER_H
#define __CLUSTER_H

/*-----------------------------------------------------------------------------
 * Redis cluster data structures, defines, exported API.
 *----------------------------------------------------------------------------*/

/* Redis集群的键空间被分割为16384个槽，集群的最大结点也是16384个*/
/* sha1算法的哈希值大小为160为，计算输出的长度为40位,  所以这里CLUSTER_NAMELEN为40 */
#define CLUSTER_SLOTS 16384
#define CLUSTER_OK 0          /* Everything looks ok */
#define CLUSTER_FAIL 1        /* The cluster can't work */
#define CLUSTER_NAMELEN 40    /* sha1 hex length */
#define CLUSTER_PORT_INCR 10000 /* Cluster port = baseport + PORT_INCR */

/* 定义了一些时间, 如果宏定义是以MULT结尾的, 表示的是timeout值的整数倍*/
/* The following defines are amount of time, sometimes expressed as
 * multiplicators of the node timeout value (when ending with MULT). */
#define CLUSTER_DEFAULT_NODE_TIMEOUT 15000
#define CLUSTER_DEFAULT_SLAVE_VALIDITY 10 /* Slave max data age factor. */
#define CLUSTER_DEFAULT_REQUIRE_FULL_COVERAGE 1
#define CLUSTER_FAIL_REPORT_VALIDITY_MULT 2 /* Fail report validity. */
#define CLUSTER_FAIL_UNDO_TIME_MULT 2 /* Undo fail if master is back. */
#define CLUSTER_FAIL_UNDO_TIME_ADD 10 /* Some additional time. */
#define CLUSTER_FAILOVER_DELAY 5 /* Seconds */
#define CLUSTER_DEFAULT_MIGRATION_BARRIER 1
#define CLUSTER_MF_TIMEOUT 5000 /* Milliseconds to do a manual failover. */
#define CLUSTER_MF_PAUSE_MULT 2 /* Master pause manual failover mult. */
#define CLUSTER_SLAVE_MIGRATION_DELAY 5000 /* Delay for slave migration. */

/* 对函数getNodeByQuery()返回值的宏定义*/
/* Redirection errors returned by getNodeByQuery(). */
#define CLUSTER_REDIR_NONE 0          /* Node can serve the request. */
#define CLUSTER_REDIR_CROSS_SLOT 1    /* -CROSSSLOT request. */
#define CLUSTER_REDIR_UNSTABLE 2      /* -TRYAGAIN redirection required */
#define CLUSTER_REDIR_ASK 3           /* -ASK redirection required. */
#define CLUSTER_REDIR_MOVED 4         /* -MOVED redirection required. */
#define CLUSTER_REDIR_DOWN_STATE 5    /* -CLUSTERDOWN, global state. */
#define CLUSTER_REDIR_DOWN_UNBOUND 6  /* -CLUSTERDOWN, unbound slot. */

struct clusterNode;

/* clusterLink这个结构体包括了所有和别的结点通信(talk with)需要的信息:
 * 包括: 创建时间、socket的文件描述符、发送和接受缓冲区等信息
*/
/* clusterLink encapsulates everything needed to talk with a remote node. */
typedef struct clusterLink {
	// 创建连接时间
    mstime_t ctime;             /* Link creation time */
	// TCP socket的文件描述符
    int fd;                     /* TCP socket file descriptor */
	// 发送缓冲区
    sds sndbuf;                 /* Packet send buffer */
	// 接受缓冲区
    sds rcvbuf;                 /* Packet reception buffer */
	// 与此连接相关联的结点，如果没有的话就为NULL
    struct clusterNode *node;   /* Node related to this link if any, or NULL */
} clusterLink;

/* 集群结点的标志和宏定义 */
/* Cluster node flags and macros. */
#define CLUSTER_NODE_MASTER 1     /* The node is a master */
#define CLUSTER_NODE_SLAVE 2      /* The node is a slave */
#define CLUSTER_NODE_PFAIL 4      /* Failure? Need acknowledge */
#define CLUSTER_NODE_FAIL 8       /* The node is believed to be malfunctioning */
#define CLUSTER_NODE_MYSELF 16    /* This node is myself */
#define CLUSTER_NODE_HANDSHAKE 32 /* We have still to exchange the first ping */
#define CLUSTER_NODE_NOADDR   64  /* We don't know the address of this node */
#define CLUSTER_NODE_MEET 128     /* Send a MEET message to this node */
#define CLUSTER_NODE_MIGRATE_TO 256 /* Master elegible for replica migration. */
#define CLUSTER_NODE_NULL_NAME "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

#define nodeIsMaster(n) ((n)->flags & CLUSTER_NODE_MASTER)
#define nodeIsSlave(n) ((n)->flags & CLUSTER_NODE_SLAVE)
#define nodeInHandshake(n) ((n)->flags & CLUSTER_NODE_HANDSHAKE)
#define nodeHasAddr(n) (!((n)->flags & CLUSTER_NODE_NOADDR))
#define nodeWithoutAddr(n) ((n)->flags & CLUSTER_NODE_NOADDR)
#define nodeTimedOut(n) ((n)->flags & CLUSTER_NODE_PFAIL)
#define nodeFailed(n) ((n)->flags & CLUSTER_NODE_FAIL)

/* slave结点不能够故障转移的原因 */
/* Reasons why a slave is not able to failover. */
#define CLUSTER_CANT_FAILOVER_NONE 0
#define CLUSTER_CANT_FAILOVER_DATA_AGE 1
#define CLUSTER_CANT_FAILOVER_WAITING_DELAY 2
#define CLUSTER_CANT_FAILOVER_EXPIRED 3
#define CLUSTER_CANT_FAILOVER_WAITING_VOTES 4
#define CLUSTER_CANT_FAILOVER_RELOG_PERIOD (60*5) /* seconds. */

/* This structure represent elements of node->fail_reports. */
typedef struct clusterNodeFailReport {
    struct clusterNode *node;  /* Node reporting the failure condition. */
    mstime_t time;             /* Time of the last report from this node. */
} clusterNodeFailReport;

typedef struct clusterNode {
	// 结点对象创建的时间
    mstime_t ctime; /* Node object creation time. */

	// 结点的名字, 由40个十六进制字符表示
	// sha1哈希算法会产生一个160为的消息摘要, 也就是40位的十六进制字符
    char name[CLUSTER_NAMELEN]; /* Node name, hex string, sha1-size */

	// 集群结点标识
	// 使用各种不同的标记记录结点的角色(比如主节点或者从结点)
	// 以及结点目前所处的状态(比如下线或者上线)
    int flags;      /* CLUSTER_NODE_... */

	// 结点的配置纪元
    uint64_t configEpoch; /* Last configEpoch observed for this node */

	// 由这个结点负责处理的槽
	// 长度为CLUSTER_SLOTS/8=16384/8=2048个
	// 每个字节的每个位记录了一个槽的保存状态
	// 位的值为1表示槽正由本节点处理，值为0则表示槽并非本节点处理
	// 比如: slots[0]的第一位保存了槽0的状态
	// slots[0]的第二位保存了槽1的保存状态,以此类推
    unsigned char slots[CLUSTER_SLOTS/8]; /* slots handled by this node */

	// 由结点负责处理的槽数量
    int numslots;   /* Number of slots handled by this node */

	// 如果本节点是master节点, 这个属性用来记录slave结点的数量
    int numslaves;  /* Number of slave nodes, if this is a master */

	// 指针数组, 指向各个从结点
    struct clusterNode **slaves; /* pointers to slave nodes */

	// 如果这是一个slave结点, 那么指向主节点
    struct clusterNode *slaveof; /* pointer to the master node. Note that it
                                    may be NULL even if the node is a slave
                                    if we don't have the master node in our
                                    tables. */

	// 最后一次发送ping命令的时间								
    mstime_t ping_sent;      /* Unix time we sent latest ping */

	// 最后一次收到pong回复的时间戳
    mstime_t pong_received;  /* Unix time we received the pong */

	// 最后一次被设置为FAIL状态的时间
    mstime_t fail_time;      /* Unix time when FAIL flag was set */

	// 最后一次给某个从结点投票的时间
    mstime_t voted_time;     /* Last time we voted for a slave of this master */

	// 最后一次从这个结点接收到复制偏移量的时间
    mstime_t repl_offset_time;  /* Unix time we received offset for this node */

	// 开始成为孤儿master结点的时间
    mstime_t orphaned_time;     /* Starting time of orphaned master condition */

	// 这个结点的复制偏移量
    long long repl_offset;      /* Last known repl offset for this node. */

	// 结点的IP地址
    char ip[NET_IP_STR_LEN];  /* Latest known IP address of this node */

	// 结点的端口号
    int port;                   /* Latest known port of this node */

	// 保存连接结点所需的有关信息以及这个连接相关联的结点
    clusterLink *link;          /* TCP/IP link with this node */

	// 一个双向链表，记录了所有其他结点对该节点的下线报告
    list *fail_reports;         /* List of nodes signaling this as failing */
} clusterNode;

typedef struct clusterState {
	// 指向当前的结点
    clusterNode *myself;  /* This node */
	
	// 集群当前的配置纪元，用于实现故障转移
    uint64_t currentEpoch;
	
	// 集群当前的状态:是在线还是下线
    int state;            /* CLUSTER_OK, CLUSTER_FAIL, ... */
	
	// 集群中至少处理一个槽的结点的数量.如果有未被处理的槽, 集群处于下线状态
    int size;             /* Num of master nodes with at least one slot */
	
	// 集群结点名单(包括myself结点)
	// 字典的键为节点的名字, 字典的值为clusterNode结构
    dict *nodes;          /* Hash table of name -> clusterNode structures */
	
	// 结点黑名单, 用于CLUSTER FORGET命令
	// 防止被FORGET的命令重新被添加到集群里面
	// (似乎没有在使用)
    dict *nodes_black_list; /* Nodes we don't re-add for a few seconds. */

	// 记录要从当前结点迁移到目标结点的槽,以及迁移的目标结点
	// migrating_slots_to[i] = NULL 表示槽i未迁移
	// migrating_slots_to[i] = clusterNode_A 表示槽i要从本结点迁移至结点A
    clusterNode *migrating_slots_to[CLUSTER_SLOTS];

	// 记录要从源结点迁移到本节点的槽, 以及进行迁移的源结点
	// importing_slots_from[i] = NULL   表示槽i未进行导入
	// importing_slots_from[i] = clusterNode_A 表示正从结点A中导入槽i
    clusterNode *importing_slots_from[CLUSTER_SLOTS];

	// 负责处理各个槽的结点
	// 例如: slots[i] = clusterNode_A  表示槽i由结点A处理
	// 每个结点的clusterNode结构中也包含了本节点所处理槽的位数组，相应位为1，则表示该slot由本节点处理
	// 维护该结构可以快速判断某个槽是否有结点处理，或者时某个槽具体在哪个结点处理
    clusterNode *slots[CLUSTER_SLOTS];

	// 跳跃表, 表中以槽作为分值, 键作为成员, 对槽进行有序排序
	// 当需要对某些槽进行区间(range)操作时, 这个跳跃表可以提供方便
	// 具体操作定义在db.c里面
    zskiplist *slots_to_keys;

	// 下面这些域被用于进行故障转移选举, master挂掉之后，选出一个slave
    /* The following fields are used to take the slave state on elections. */

	// 上次或者下次执行选举的时间
    mstime_t failover_auth_time; /* Time of previous or next election. */

	// 结点获得的投票数量
    int failover_auth_count;    /* Number of votes received so far. */

	// 如果值为1,, 表示本节点已经向其他结点发送了投票请求
    int failover_auth_sent;     /* True if we already asked for votes. */
    int failover_auth_rank;     /* This slave rank for current auth request. */
    uint64_t failover_auth_epoch; /* Epoch of the current election. */
    int cant_failover_reason;   /* Why a slave is currently not able to
                                   failover. See the CANT_FAILOVER_* macros. */

	// 手动故障转移执行的时间限制
    /* Manual failover state in common. */
    mstime_t mf_end;            /* Manual failover time limit (ms unixtime).
                                   It is zero if there is no MF in progress. */

	// 主服务器的故障转移状态
    /* Manual failover state of master. */
    clusterNode *mf_slave;      /* Slave performing the manual failover. */
								   
	// 从服务器的故障转移状态
    /* Manual failover state of slave. */
    long long mf_master_offset; /* Master offset the slave needs to start MF
                                   or zero if stil not received. */
								   	
	// 手动故障转移是否可以开始的标志位
	// 值为非0 表示各个主服务器可以开始投票
    int mf_can_start;           /* If non-zero signal that the manual failover
                                   can start requesting masters vote. */

	// 下面这些域被主服务器使用，用来记录选举的状态
    /* The followign fields are used by masters to take state on elections. */
	// 集群最后一次投票的纪元
    uint64_t lastVoteEpoch;     /* Epoch of the last vote granted. */
	// 在进入下个时间循环之前要做的事情
    int todo_before_sleep; /* Things to do in clusterBeforeSleep(). */
	// 通过cluster 连接发送的消息数量
    long long stats_bus_messages_sent;  /* Num of msg sent via cluster bus. */
	// 通过cluster 连接接收到的的消息数量
    long long stats_bus_messages_received; /* Num of msg rcvd via cluster bus.*/
} clusterState;

/* clusterState todo_before_sleep flags. */
#define CLUSTER_TODO_HANDLE_FAILOVER (1<<0)
#define CLUSTER_TODO_UPDATE_STATE (1<<1)
#define CLUSTER_TODO_SAVE_CONFIG (1<<2)
#define CLUSTER_TODO_FSYNC_CONFIG (1<<3)

/* Redis cluster messages header */

/* Note that the PING, PONG and MEET messages are actually the same exact
 * kind of packet. PONG is the reply to ping, in the exact format as a PING,
 * while MEET is a special PING that forces the receiver to add the sender
 * as a node (if it is not already in the list). */
#define CLUSTERMSG_TYPE_PING 0          /* Ping */
#define CLUSTERMSG_TYPE_PONG 1          /* Pong (reply to Ping) */
#define CLUSTERMSG_TYPE_MEET 2          /* Meet "let's join" message */
#define CLUSTERMSG_TYPE_FAIL 3          /* Mark node xxx as failing */
#define CLUSTERMSG_TYPE_PUBLISH 4       /* Pub/Sub Publish propagation */
#define CLUSTERMSG_TYPE_FAILOVER_AUTH_REQUEST 5 /* May I failover? */
#define CLUSTERMSG_TYPE_FAILOVER_AUTH_ACK 6     /* Yes, you have my vote */
#define CLUSTERMSG_TYPE_UPDATE 7        /* Another node slots configuration */
#define CLUSTERMSG_TYPE_MFSTART 8       /* Pause clients for manual failover */

/* Initially we don't know our "name", but we'll find it once we connect
 * to the first node, using the getsockname() function. Then we'll use this
 * address for all the next messages. */
typedef struct {
    char nodename[CLUSTER_NAMELEN];
    uint32_t ping_sent;
    uint32_t pong_received;
    char ip[NET_IP_STR_LEN];  /* IP address last time it was seen */
    uint16_t port;              /* port last time it was seen */
    uint16_t flags;             /* node->flags copy */
    uint16_t notused1;          /* Some room for future improvements. */
    uint32_t notused2;
} clusterMsgDataGossip;

typedef struct {
    char nodename[CLUSTER_NAMELEN];
} clusterMsgDataFail;

typedef struct {
    uint32_t channel_len;
    uint32_t message_len;
    /* We can't reclare bulk_data as bulk_data[] since this structure is
     * nested. The 8 bytes are removed from the count during the message
     * length computation. */
    unsigned char bulk_data[8];
} clusterMsgDataPublish;

typedef struct {
    uint64_t configEpoch; /* Config epoch of the specified instance. */
    char nodename[CLUSTER_NAMELEN]; /* Name of the slots owner. */
    unsigned char slots[CLUSTER_SLOTS/8]; /* Slots bitmap. */
} clusterMsgDataUpdate;

union clusterMsgData {
    /* PING, MEET and PONG */
    struct {
        /* Array of N clusterMsgDataGossip structures */
        clusterMsgDataGossip gossip[1];
    } ping;

    /* FAIL */
    struct {
        clusterMsgDataFail about;
    } fail;

    /* PUBLISH */
    struct {
        clusterMsgDataPublish msg;
    } publish;

    /* UPDATE */
    struct {
        clusterMsgDataUpdate nodecfg;
    } update;
};

#define CLUSTER_PROTO_VER 0 /* Cluster bus protocol version. */

// clusterMsg结构体是消息的头部, 真正的数据部分在clusterMsgData里面
// clusterMsgData是clusterMsg结构体的一个成员
typedef struct {
	// 消息签名, 对于cluster消息, 固定为字符序列RCmb
    char sig[4];        /* Siganture "RCmb" (Redis Cluster message bus). */
	// 消息的总长度
    uint32_t totlen;    /* Total length of this message */
	// 协议的版本, 当前设置为0
    uint16_t ver;       /* Protocol version, currently set to 0. */
	// 两个字节未使用
    uint16_t notused0;  /* 2 bytes not used. */
	// 消息类型
    uint16_t type;      /* Message type */
	// 数量: 只用于一些种类的message
    uint16_t count;     /* Only used for some kind of messages. */
	// 根据发送结点的纪元
    uint64_t currentEpoch;  /* The epoch accordingly to the sending node. */
	// 如果是master结点, 这个是配置纪元; 如果是slave结点, 这是上一个被master建议的纪元
	// (没看懂这是用来干嘛的)
    uint64_t configEpoch;   /* The config epoch if it's a master, or the last
                               epoch advertised by its master if it is a
                               slave. */
    // 偏移量                     
    uint64_t offset;    /* Master replication offset if node is a master or
                           processed replication offset if node is a slave. */
	// 发送结点的名字
    char sender[CLUSTER_NAMELEN]; /* Name of the sender node */
	// 结点维护的槽的状态位					   
    unsigned char myslots[CLUSTER_SLOTS/8];
	// 如果本节点是slave结点,则slaveof记录对应的master结点ID
    char slaveof[CLUSTER_NAMELEN];
    char notused1[32];  /* 32 bytes reserved for future usage. */
    uint16_t port;      /* Sender TCP base port */
    uint16_t flags;     /* Sender node flags */
	// 集群状态
    unsigned char state; /* Cluster state from the POV of the sender */
    unsigned char mflags[3]; /* Message flags: CLUSTERMSG_FLAG[012]_... */
    union clusterMsgData data;
} clusterMsg;

// 集群消息的最小长度, 因为数据部分是放在clusterMsgData结构体中
// 所有最小长度就是clusterMsg-clusterMsgData.
#define CLUSTERMSG_MIN_LEN (sizeof(clusterMsg)-sizeof(union clusterMsgData))

/* Message flags better specify the packet content or are used to
 * provide some information about the node state. */
#define CLUSTERMSG_FLAG0_PAUSED (1<<0) /* Master paused for manual failover. */
#define CLUSTERMSG_FLAG0_FORCEACK (1<<1) /* Give ACK to AUTH_REQUEST even if
                                            master is up. */

/* ---------------------- API exported outside cluster.c -------------------- */
clusterNode *getNodeByQuery(client *c, struct redisCommand *cmd, robj **argv, int argc, int *hashslot, int *ask);
int clusterRedirectBlockedClientIfNeeded(client *c);
void clusterRedirectClient(client *c, clusterNode *n, int hashslot, int error_code);

#endif /* __CLUSTER_H */
