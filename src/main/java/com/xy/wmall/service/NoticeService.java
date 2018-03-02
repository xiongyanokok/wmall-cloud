package com.xy.wmall.service;

import com.xy.wmall.model.Notice;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年03月02日 下午01:51:53
 */
public interface NoticeService extends BaseService<Notice> {

	/**
	 * 获取最新通知
	 * 
	 * @return
	 */
	Notice getNewestNotice();
	
}
