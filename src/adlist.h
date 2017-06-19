/* adlist.h - A generic doubly linked list implementation
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

#ifndef __ADLIST_H__
#define __ADLIST_H__

/* Node, List, and Iterator are the only data structures used currently. */

/*
 * 链表节点
 * 按理说直接使用listNode就直接能够构成链表结构但是使用list结构体操作会更加的方便
 */
typedef struct listNode {
    // 长一个节点
    struct listNode *prev;
    struct listNode *next;
    // 节点的具体值指针，由于为void*，所以链表中节点的值类型可以完全各不相同
    void *value;
} listNode;

/*
 * 链表的迭代器，仅用于迭代链表
 * 这个迭代器从设计上说比较简陋没有迭代功能，只有调用listNext()函数进行顺序迭代
 * 实际上由于listNode本身记录了自己的上下节点加上迭代方向，本身也是可以进行顺序迭代的
 */
typedef struct listIter {
    listNode *next;
    // 迭代的方向：为宏AL_START_HEAD(0)时代表从表头开始遍历，为宏AL_START_TAIL(1)时代表从表尾开始
    int direction;
} listIter;

/*
 * 链表
 * 虽然listNode本身可以表示链表，但是list结构体操作更加方便并且记录了一些关键信息，降低了查询复杂度
 */
typedef struct list {
    // 链表的首个元素
    listNode *head;
    // 链表的尾部元素
    // 之所以记录尾部元素是因为可以方便的支持redis能够通过负数表示从尾部倒数索引
    listNode *tail;
    // 节点拷贝函数,在对链表进行复制时会尝试调用该函数
    // 如果没有设置该函数则仅会对链表进行浅拷贝（直接拷贝将值的地址赋给新链表节点）
    void *(*dup)(void *ptr);
    // 在释放链表节点元素内存前会尝试调用该函数,相当于节点销毁前的一个监听
    void (*free)(void *ptr);
    // 在搜索链表中节点时会调用该函数来判断两个节点是否相等
    // 如果没有设置该函数则会直接比较两个节点的内存地址
    int (*match)(void *ptr, void *key);
    // 链表当前的节点个数，即链表长度，方便统计(因为有提供给用户获取链表长度的命令llen)
    unsigned long len;
} list;

/* Functions implemented as macros */
#define listLength(l) ((l)->len)
#define listFirst(l) ((l)->head)
#define listLast(l) ((l)->tail)
#define listPrevNode(n) ((n)->prev)
#define listNextNode(n) ((n)->next)
#define listNodeValue(n) ((n)->value)

#define listSetDupMethod(l,m) ((l)->dup = (m))
#define listSetFreeMethod(l,m) ((l)->free = (m))
#define listSetMatchMethod(l,m) ((l)->match = (m))

#define listGetDupMethod(l) ((l)->dup)
#define listGetFree(l) ((l)->free)
#define listGetMatchMethod(l) ((l)->match)

/* Prototypes */
list *listCreate(void);
void listRelease(list *list);
void listEmpty(list *list);
list *listAddNodeHead(list *list, void *value);
list *listAddNodeTail(list *list, void *value);
list *listInsertNode(list *list, listNode *old_node, void *value, int after);
void listDelNode(list *list, listNode *node);
listIter *listGetIterator(list *list, int direction);
listNode *listNext(listIter *iter);
void listReleaseIterator(listIter *iter);
list *listDup(list *orig);
listNode *listSearchKey(list *list, void *key);
listNode *listIndex(list *list, long index);
void listRewind(list *list, listIter *li);
void listRewindTail(list *list, listIter *li);
void listRotate(list *list);
void listJoin(list *l, list *o);

/* Directions for iterators */
#define AL_START_HEAD 0
#define AL_START_TAIL 1

#endif /* __ADLIST_H__ */
