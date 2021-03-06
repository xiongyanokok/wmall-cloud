package com.xy.wmall.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Menu;
import com.xy.wmall.model.Role;
import com.xy.wmall.model.RoleMenu;
import com.xy.wmall.service.MenuService;
import com.xy.wmall.service.RoleMenuService;
import com.xy.wmall.service.RoleService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:26
 */
@Controller
@RequestMapping(value = "/admin/role", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class RoleController extends BaseController {

    @Autowired
	private RoleService roleService;
    
    @Autowired
    private MenuService menuService;
    
    @Autowired
    private RoleMenuService roleMenuService;
    
	
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
			// 角色名称
			map.put("name", request.getParameter("name"));
			return roleService.listByMap(map);
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
		log.info("【{}】保存成功", role);
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
		Role role = roleService.getById(id);
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
		Role roleInfo = roleService.getById(role.getId());
		Assert.notNull(roleInfo, "数据不存在");
		role.setUpdateUserId(getUserId());
		role.setUpdateTime(new Date());
		roleService.update(role);
		log.info("【{}】修改成功", role);
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
		Role role = roleService.getById(id);
		Assert.notNull(role, "数据不存在");
		roleService.remove(role);
		log.info("【{}】删除成功", role);
		return buildSuccess("删除成功");
	}
	
	/**
	 * 角色权限
	 * 
	 * @param model
	 * @param roleId
	 * @return
	 */
	@RequestMapping(value = "/menu", method = { RequestMethod.GET })
	public String menu(Model model, Integer roleId) {
		Assert.notNull(roleId, "roleId为空");
		Role role = roleService.getById(roleId);
		Assert.notNull(role, "数据不存在");
		model.addAttribute("role", role);
		
		// 根据角色查询权限 
		List<Integer> menuIds = roleMenuService.listMenuByRoleId(roleId);
		
		// 查询权限列表
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		map.put("orderBy", "sort, create_time");
		List<Menu> menus = menuService.listByMap(map);
		if (CollectionUtils.isNotEmpty(menus) && CollectionUtils.isNotEmpty(menuIds)) {
			for (Menu menu : menus) {
				menu.setChecked(menuIds.contains(menu.getId()));
			}
		}
		model.addAttribute("menus", menus);
		return "role/menu";
	}
	
	/**
	 * 角色权限
	 * 
	 * @param id
	 * @param roleId
	 * @return
	 */
	@RequestMapping(value = "/role_menu", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> roleMenu(Integer roleId, Integer[] menuId) {
		Assert.notNull(roleId, "roleId为空");
		Assert.notNull(menuId, "menuId为空");
		
		// 删除角色权限
		roleMenuService.deleteByRoleId(roleId);
		
		// 分配角色权限
		List<RoleMenu> roleMenus = new ArrayList<>();
		for (Integer id : menuId) {
			RoleMenu roleMenu = new RoleMenu();
			roleMenu.setRoleId(roleId);
			roleMenu.setMenuId(id);
			roleMenus.add(roleMenu);
		}
		roleMenuService.batchSave(roleMenus);
		return buildSuccess("权限分配成功");
	}
	
}
