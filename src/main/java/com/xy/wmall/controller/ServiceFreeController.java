package com.xy.wmall.controller;

import java.util.Arrays;
import java.util.Date;
import java.util.Map;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.Constant;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.ServiceFree;
import com.xy.wmall.model.User;
import com.xy.wmall.service.ServiceFreeService;
import com.xy.wmall.service.UserService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
@Controller
@RequestMapping(value = "/admin/servicefree", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class ServiceFreeController extends BaseController {

    @Autowired
	private ServiceFreeService serviceFreeService;
    
    @Autowired
    private UserService userService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @param userId
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model, Integer userId) {
		Assert.notNull(userId, "userId为空");
		User user = userService.getById(userId);
		Assert.notNull(user, "数据不存在");
		model.addAttribute("user", user);
		return "servicefree/list";
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
			// userId
			map.put("userId", request.getParameter("userId"));
			return serviceFreeService.listByMap(map);
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @param userId
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model, Integer userId) {
		Assert.notNull(userId, "userId为空");
		User user = userService.getById(userId);
		Assert.notNull(user, "数据不存在");
		model.addAttribute("user", user);
		return "servicefree/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param serviceFree
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(ServiceFree serviceFree) {
		Assert.notNull(serviceFree, "保存数据为空");
		// 获取用户服务截止日期
		Map<Integer, Date> userServiceMap = serviceFreeService.listServiceDate(Arrays.asList(serviceFree.getUserId()));
		Date serviceDate = null;
		if (MapUtils.isEmpty(userServiceMap)) {
			// 免费试用30天
			User user = userService.getById(serviceFree.getUserId());
			serviceDate = DateUtils.addDays(user.getCreateTime(), Constant.FREE_30_DAY);
		} else {
			// 服务截止日期
			serviceDate = userServiceMap.get(serviceFree.getUserId());
		}
		// 服务开始日期
		Date startDate = DateUtils.addDays(serviceDate, 1);
		// 服务截止日期
		Date endDate = DateUtils.addYears(serviceDate, 1);
		serviceFree.setStartDate(startDate);
		serviceFree.setEndDate(endDate);
		serviceFree.setCreateUserId(getUserId());
		serviceFree.setCreateTime(new Date());
		serviceFree.setUpdateUserId(getUserId());
		serviceFree.setUpdateTime(new Date());
		serviceFree.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		serviceFreeService.save(serviceFree);
		log.info("【{}】保存成功", serviceFree);
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
		ServiceFree serviceFree = serviceFreeService.getById(id);
		Assert.notNull(serviceFree, "数据不存在");
		model.addAttribute("serviceFree", serviceFree);
		User user = userService.getById(serviceFree.getUserId());
		Assert.notNull(user, "数据不存在");
		model.addAttribute("user", user);
		return "servicefree/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param serviceFree
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(ServiceFree serviceFree) {
		Assert.notNull(serviceFree, "修改数据为空");
		ServiceFree serviceFreeInfo = serviceFreeService.getById(serviceFree.getId());
		Assert.notNull(serviceFreeInfo, "数据不存在");
		serviceFree.setUpdateUserId(getUserId());
		serviceFree.setUpdateTime(new Date());
		serviceFreeService.update(serviceFree);
		log.info("【{}】修改成功", serviceFree);
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
		ServiceFree serviceFree = serviceFreeService.getById(id);
		Assert.notNull(serviceFree, "数据不存在");
		serviceFreeService.remove(serviceFree);
		log.info("【{}】删除成功", serviceFree);
		return buildSuccess("删除成功");
	}
	
}
