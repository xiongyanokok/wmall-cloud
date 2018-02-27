package com.xy.wmall.controller;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.JacksonUtils;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Menu;
import com.xy.wmall.service.MenuService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
@Controller
@RequestMapping(value = "/admin/menu", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class MenuController extends BaseController {

    @Autowired
	private MenuService menuService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		model.addAttribute("parentId", 0);
		return "menu/list";
	}
	
	/**
	 * 进入列表页面
	 * 
	 * @param parentId
	 * @return
	 */
	@RequestMapping(value = "/sub_list", method = { RequestMethod.GET })
	public String subList(Model model, Integer parentId) {
		Assert.notNull(parentId, "parentId为空");
		model.addAttribute("parentId", parentId);
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		map.put("parentId", 0);
		List<Menu> menus = menuService.listByMap(map);
		model.addAttribute("menus", menus);
		model.addAttribute("menusJson", JacksonUtils.serialize(menus));
		return "menu/sub_list";
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
			// 父菜单ID
			map.put("parentId", request.getParameter("parentId"));
			// 菜单名称
			map.put("name", request.getParameter("name"));
			// 菜单地址
			map.put("uri", request.getParameter("uri"));
			return menuService.listByMap(map);
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param parentId
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model, Integer parentId) {
		Assert.notNull(parentId, "parentId为空");
		model.addAttribute("parentId", parentId);
		if (parentId > 0) {
			Menu menu = menuService.getById(parentId);
			Assert.notNull(menu, "数据不存在");
			model.addAttribute("supName", menu.getName());
		}
		return "menu/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param menu
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Menu menu) {
		Assert.notNull(menu, "保存数据为空");
		menu.setCreateUserId(getUserId());
		menu.setCreateTime(new Date());
		menu.setUpdateUserId(getUserId());
		menu.setUpdateTime(new Date());
		menu.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		menuService.save(menu);
		log.info("【{}】保存成功", menu);
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
		Menu menu = menuService.getById(id);
		Assert.notNull(menu, "数据不存在");
		model.addAttribute("menu", menu);
		if (menu.getParentId() > 0) {
			Menu supMenu = menuService.getById(menu.getParentId());
			Assert.notNull(supMenu, "数据不存在");
			model.addAttribute("supName", supMenu.getName());
		}
		return "menu/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param menu
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Menu menu) {
		Assert.notNull(menu, "修改数据为空");
		Menu menuInfo = menuService.getById(menu.getId());
		Assert.notNull(menuInfo, "数据不存在");
		menu.setUpdateUserId(getUserId());
		menu.setUpdateTime(new Date());
		menuService.update(menu);
		log.info("【{}】修改成功", menu);
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
		Menu menu = menuService.getById(id);
		Assert.notNull(menu, "数据不存在");
		menuService.remove(menu);
		log.info("【{}】删除成功", menu);
		return buildSuccess("删除成功");
	}
	
}
