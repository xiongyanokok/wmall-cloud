package com.xy.wmall.service.impl;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.github.pagehelper.PageHelper;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.NoticeMapper;
import com.xy.wmall.model.Notice;
import com.xy.wmall.service.NoticeService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年03月02日 下午01:51:53
 */
@Service
public class NoticeServiceImpl extends BaseServiceImpl<NoticeMapper, Notice> implements NoticeService {

	@Autowired
	private NoticeMapper noticeMapper;
	
	/**
	 * 获取最新通知
	 * 
	 * @return
	 */
	public Notice getNewestNotice() {
		try {
			PageHelper.startPage(1, 1);
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("orderBy", "create_time desc");
	    	return noticeMapper.getByMap(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "获取最新通知失败", e);
		}
	}
	
}
