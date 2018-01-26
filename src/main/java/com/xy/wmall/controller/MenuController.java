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
import com.xy.wmall.model.Menu;
import com.xy.wmall.service.MenuService;

/**
 * Controller
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:01
 */
@Controller
@RequestMapping(value = "/admin/menu", produces = { "application/json; charset=UTF-8" })
public class MenuController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(MenuController.class);

    @Autowired
	private MenuService menuService;
	
	/**
	 * 进入列表页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "menu/list";
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
			return menuService.listMenu(map);
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
		logger.info("【{}】保存成功", menu);
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
		Menu menu = menuService.getMenuById(id);
		Assert.notNull(menu, "数据不存在");
		model.addAttribute("menu", menu);
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
		Menu menuInfo = menuService.getMenuById(menu.getId());
		Assert.notNull(menuInfo, "数据不存在");
		menu.setUpdateUserId(getUserId());
		menu.setUpdateTime(new Date());
		menuService.update(menu);
		logger.info("【{}】修改成功", menu);
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
		Menu menu = menuService.getMenuById(id);
		Assert.notNull(menu, "数据不存在");
		menuService.remove(menu);
		logger.info("【{}】删除成功", menu);
		return buildSuccess("删除成功");
	}
	
}
