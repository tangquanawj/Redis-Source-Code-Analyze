/* Hash Tables Implementation.
 *
 * This file implements in-memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto-resize if needed
 * tables of power of two in size are used, collisions are handled by
 * chaining. See the source code for more information... :)
 *
 * Copyright (c) 2006-2012, Salvatore Sanfilippo <antirez at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdint.h>

#ifndef __DICT_H
#define __DICT_H

#define DICT_OK 0
#define DICT_ERR 1

// 未使用的变量, 通过强制转换为void的方式,消除warning信息
/* Unused arguments generate annoying warnings... */
#define DICT_NOTUSED(V) ((void) V)

// 哈希表结点:每个实例中记录了一个键值对, 同时还有一个指针next, 这个指针next用于解决键值相同的hash冲突问题, 
typedef struct dictEntry {
    void *key;
    union {
        void *val;
        uint64_t u64;
        int64_t s64;
        double d;
    } v;
	// 这个指针可以将多个哈希值相同的键值对连接在一起, 以此来解决"键冲突"的问题
	// 解决的是"键冲突"的问题
    struct dictEntry *next;
} dictEntry;

// 主要是包括了对dict操作的一个结构体
typedef struct dictType {
	// 函数指针:hashFunction, 函数参数接受一个key指针
    unsigned int (*hashFunction)(const void *key);
	// 函数指针: keyDup, 估计是复制key值
    void *(*keyDup)(void *privdata, const void *key);
	// 函数指针: valDup, 估计是复制val值
    void *(*valDup)(void *privdata, const void *obj);
	// 函数指针: 对比key值
    int (*keyCompare)(void *privdata, const void *key1, const void *key2);
	// 函数指针: key值销毁器
    void (*keyDestructor)(void *privdata, void *key);
	// 函数指针: val值销毁器
    void (*valDestructor)(void *privdata, void *obj);
} dictType;

// 哈希表的结构
/* This is our hash table structure. Every dictionary has two of this as we
 * implement incremental rehashing, for the old to the new table. */
typedef struct dictht {
	// 哈希表数组
    dictEntry **table;
	// 哈希表大小
    unsigned long size;
	// 哈希表大小掩码, 用于计算索引值
    unsigned long sizemask;
	// 该哈希表已有结点的数量
    unsigned long used;
} dictht;

// Redis中的字典结构
// type属性和privdata属性是针对不同类型的键值对, 为创建多态字典而设置的。
typedef struct dict {
	// 类型特定函数
    dictType *type;
	// 私有数据
    void *privdata;
	// 哈希表
	// 一般情况下,字典只使用ht[0]哈希表, ht[1]哈希表只会在对ht[0]哈希表进行rehash时使用
	// 另一个和rehash有关的属性就是rehashidx, 它记录了rehash目前的进度,如果目前没有在进行rehash, rehashidx的值为-1.
    dictht ht[2];
	// rehash索引
	// 当rehash不在进行时, 值为-1
    long rehashidx; /* rehashing not in progress if rehashidx == -1 */
    int iterators; /* number of iterators currently running */
} dict;

// 如果safe被设置为1, 那么这是一个安全的迭代器, 可以通过dictAdd()、dictFind()、和其他函数对字典进行操作
// 即使正在迭代, 也可以使用
// 如果safe被设置为0, 那么这是一个不安全的迭代器, 在迭代的过程中, 只能够使用dictNext()
/* If safe is set to 1 this is a safe iterator, that means, you can call
 * dictAdd, dictFind, and other functions against the dictionary even while
 * iterating. Otherwise it is a non safe iterator, and only dictNext()
 * should be called while iterating. */
typedef struct dictIterator {
    dict *d;
    long index;
    int table, safe;
    dictEntry *entry, *nextEntry;
	// 这个标志是用来滥用检查的
    /* unsafe iterator fingerprint for misuse detection. */
    long long fingerprint;
} dictIterator;

// 字典扫描方法
typedef void (dictScanFunction)(void *privdata, const dictEntry *de);

// 每个hash表的初始化大小都为4
/* This is the initial size of every hash table */
#define DICT_HT_INITIAL_SIZE     4

/* ------------------------------- Macros ------------------------------------*/
#define dictFreeVal(d, entry) \
    if ((d)->type->valDestructor) \
        (d)->type->valDestructor((d)->privdata, (entry)->v.val)

#define dictSetVal(d, entry, _val_) do { \
    if ((d)->type->valDup) \
        entry->v.val = (d)->type->valDup((d)->privdata, _val_); \
    else \
        entry->v.val = (_val_); \
} while(0)

#define dictSetSignedIntegerVal(entry, _val_) \
    do { entry->v.s64 = _val_; } while(0)

#define dictSetUnsignedIntegerVal(entry, _val_) \
    do { entry->v.u64 = _val_; } while(0)

#define dictSetDoubleVal(entry, _val_) \
    do { entry->v.d = _val_; } while(0)

#define dictFreeKey(d, entry) \
    if ((d)->type->keyDestructor) \
        (d)->type->keyDestructor((d)->privdata, (entry)->key)

#define dictSetKey(d, entry, _key_) do { \
    if ((d)->type->keyDup) \
        entry->key = (d)->type->keyDup((d)->privdata, _key_); \
    else \
        entry->key = (_key_); \
} while(0)

#define dictCompareKeys(d, key1, key2) \
    (((d)->type->keyCompare) ? \
        (d)->type->keyCompare((d)->privdata, key1, key2) : \
        (key1) == (key2))

#define dictHashKey(d, key) (d)->type->hashFunction(key)
#define dictGetKey(he) ((he)->key)
#define dictGetVal(he) ((he)->v.val)
#define dictGetSignedIntegerVal(he) ((he)->v.s64)
#define dictGetUnsignedIntegerVal(he) ((he)->v.u64)
#define dictGetDoubleVal(he) ((he)->v.d)
#define dictSlots(d) ((d)->ht[0].size+(d)->ht[1].size)
#define dictSize(d) ((d)->ht[0].used+(d)->ht[1].used)
#define dictIsRehashing(d) ((d)->rehashidx != -1)

/* API */
// 创建一个新的字典
dict *dictCreate(dictType *type, void *privDataPtr);
// 扩大一个字典, 估计是用在渐进性hash中
int dictExpand(dict *d, unsigned long size);
// 在hash表中增加一个dict
int dictAdd(dict *d, void *key, void *val);
// 增加一个字典, 但是字典没有value值, 使用者可以在他想加的时候加入这个value值
dictEntry *dictAddRaw(dict *d, void *key);
// 替换一个dict的value, 如果找到匹配的key则替换, 如果没找到匹配的key值, 直接添加这个key和val的字典;
// 如果找到了, 替换该key的value值
int dictReplace(dict *d, void *key, void *val);
// 和dictAddRaw()类似,但不管是否匹配得到, 都会返回entry
dictEntry *dictReplaceRaw(dict *d, void *key);
// 查找并删除和key对应的dict
int dictDelete(dict *d, const void *key);
// 删除但是不释放空间
int dictDeleteNoFree(dict *d, const void *key);
// 释放dict的空间
void dictRelease(dict *d);
// 查找一个dict
dictEntry * dictFind(dict *d, const void *key);
void *dictFetchValue(dict *d, const void *key);
int dictResize(dict *d);
dictIterator *dictGetIterator(dict *d);
dictIterator *dictGetSafeIterator(dict *d);
dictEntry *dictNext(dictIterator *iter);
void dictReleaseIterator(dictIterator *iter);
dictEntry *dictGetRandomKey(dict *d);
unsigned int dictGetSomeKeys(dict *d, dictEntry **des, unsigned int count);
void dictGetStats(char *buf, size_t bufsize, dict *d);
unsigned int dictGenHashFunction(const void *key, int len);
unsigned int dictGenCaseHashFunction(const unsigned char *buf, int len);
void dictEmpty(dict *d, void(callback)(void*));
void dictEnableResize(void);
void dictDisableResize(void);
int dictRehash(dict *d, int n);
int dictRehashMilliseconds(dict *d, int ms);
void dictSetHashFunctionSeed(unsigned int initval);
unsigned int dictGetHashFunctionSeed(void);
unsigned long dictScan(dict *d, unsigned long v, dictScanFunction *fn, void *privdata);

// 哈希表的类型
/* Hash table types */
extern dictType dictTypeHeapStringCopyKey;
extern dictType dictTypeHeapStrings;
extern dictType dictTypeHeapStringCopyKeyValue;

#endif /* __DICT_H */
