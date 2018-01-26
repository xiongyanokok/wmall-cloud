package com.xy.wmall.controller;

import java.util.Date;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Role;
import com.xy.wmall.service.RoleService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:13
 */
@Controller
@RequestMapping(value = "/admin/role", produces = { "application/json; charset=UTF-8" })
public class RoleController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(RoleController.class);

    @Autowired
	private RoleService roleService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "role/list";
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
			return roleService.listRole(map);
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
		return "role/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param role
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Role role) {
		Assert.notNull(role, "保存数据为空");
		role.setCreateUserId(getUserId());
		role.setCreateTime(new Date());
		role.setUpdateUserId(getUserId());
		role.setUpdateTime(new Date());
		role.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		roleService.save(role);
		logger.info("【{}】保存成功", role);
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
		Role role = roleService.getRoleById(id);
		Assert.notNull(role, "数据不存在");
		model.addAttribute("role", role);
		return "role/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param role
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Role role) {
		Assert.notNull(role, "修改数据为空");
		Role roleInfo = roleService.getRoleById(role.getId());
		Assert.notNull(roleInfo, "数据不存在");
		role.setUpdateUserId(getUserId());
		role.setUpdateTime(new Date());
		roleService.update(role);
		logger.info("【{}】修改成功", role);
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
		Role role = roleService.getRoleById(id);
		Assert.notNull(role, "数据不存在");
		roleService.remove(role);
		logger.info("【{}】删除成功", role);
		return buildSuccess("删除成功");
	}
	
}