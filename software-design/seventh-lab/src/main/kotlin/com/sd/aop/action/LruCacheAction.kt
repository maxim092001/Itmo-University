package com.sd.aop.action

sealed interface LruCacheAction

class Put(val key: Int, val value: Int) : LruCacheAction

class Get(val key: Int) : LruCacheAction

object IsEmpty : LruCacheAction

object Size : LruCacheAction

class Contains(val key: Int) : LruCacheAction