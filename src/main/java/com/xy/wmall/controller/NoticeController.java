package com.xy.wmall.controller;

import java.util.Date;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Notice;
import com.xy.wmall.service.NoticeService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年03月02日 下午01:51:53
 */
@Controller
@RequestMapping(value = "/admin/notice", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class NoticeController extends BaseController {

    @Autowired
	private NoticeService noticeService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "notice/list";
	}
	
	/**
	 * 列表分页查询
	 * 
	 * @return
	 */
	@RequestMapping(value = "/query", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> query() {
		return pageInfoResult(map -> {
			// 查询条件
			// 通知内容
			map.put("content", request.getParameter("content"));
			return noticeService.listByMap(map);
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model) {
		return "notice/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param notice
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Notice notice) {
		Assert.notNull(notice, "保存数据为空");
		notice.setCreateUserId(getUserId());
		notice.setCreateTime(new Date());
		notice.setUpdateUserId(getUserId());
		notice.setUpdateTime(new Date());
		notice.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		noticeService.save(notice);
		log.info("【{}】保存成功", notice);
		return buildSuccess("保存成功");
	}
	
	/**
	 * 进入修改页面
	 * 
	 * @param model
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/edit", method = { RequestMethod.GET })
	public String edit(Model model, Integer id) {
		Assert.notNull(id, "id为空");
		Notice notice = noticeService.getById(id);
		Assert.notNull(notice, "数据不存在");
		model.addAttribute("notice", notice);
		return "notice/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param notice
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Notice notice) {
		Assert.notNull(notice, "修改数据为空");
		Notice noticeInfo = noticeService.getById(notice.getId());
		Assert.notNull(noticeInfo, "数据不存在");
		notice.setUpdateUserId(getUserId());
		notice.setUpdateTime(new Date());
		noticeService.update(notice);
		log.info("【{}】修改成功", notice);
		return buildSuccess("修改成功");
	}
	
	/**
	 * 删除数据
	 * 
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/delete", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> delete(Integer id) {
		Assert.notNull(id, "id为空");
		Notice notice = noticeService.getById(id);
		Assert.notNull(notice, "数据不存在");
		noticeService.remove(notice);
		log.info("【{}】删除成功", notice);
		return buildSuccess("删除成功");
	}
	
}
