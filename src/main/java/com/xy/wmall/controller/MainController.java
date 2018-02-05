package com.xy.wmall.controller;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Constant;
import com.xy.wmall.common.utils.DateUtils;
import com.xy.wmall.common.utils.Md5Utils;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.model.Menu;
import com.xy.wmall.model.Proxy;
import com.xy.wmall.model.User;
import com.xy.wmall.model.VerifyCode;
import com.xy.wmall.pojo.UserInfo;
import com.xy.wmall.service.MenuService;
import com.xy.wmall.service.ProxyService;
import com.xy.wmall.service.UserService;
import com.xy.wmall.service.VerifyCodeService;

/**
 * Controller
 * 
 * @author admin
 * @date 2016年11月17日 下午2:06:29
 */
@Controller
@RequestMapping(value = "/", produces = {"application/json; charset=UTF-8"})
public class MainController extends BaseController {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(MainController.class);
	
	@Autowired
	private UserService userService;
	
	@Autowired
    private ProxyService proxyService;
	
	@Autowired
	private MenuService menuService;
	
	@Autowired
    private VerifyCodeService verifyCodeService;
	
    /**
	 * 登录页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/", method = {RequestMethod.GET})
	public String index(Model model) {
		return "system/login";
	}
    
    /**
	 * 用户登录
	 * 
	 * @param username
	 * @param password
	 * @param checkCode
	 * @return
	 */
	@RequestMapping(value = "/login", method = {RequestMethod.POST})
	@ResponseBody
	public Map<String, Object> login(String username, String password, String checkCode) {
		if (StringUtils.isAnyEmpty(username, password, checkCode)) {
			logger.error("登录失败：用户名或密码或验证码为空");
			throw new WmallException("登录失败");
		}
		
		// 验证验证码
		String imageCode = (String) session.getAttribute(Constant.IMAGE_CODE);
		if (StringUtils.isEmpty(imageCode) || !imageCode.equalsIgnoreCase(checkCode)) {
			logger.error("验证码错误：session验证码【{}】，登录验证码【{}】", imageCode, checkCode);
			return buildFail("验证码错误");
		}
		// 清除验证码session
		session.removeAttribute(Constant.IMAGE_CODE);
		
		// 查询用户
		User user = userService.getUserByUsername(username);
		if (null == user || !user.getPassword().equals(Md5Utils.md5(password))) {
			logger.error("用户名或密码错误：用户名【{}】，密码【{}】", username, password);
			return buildFail("用户名或密码错误");
		}
		if (user.getDisabled()) {
			logger.error("用户已被禁用：用户名【{}】，密码【{}】", username, password);
			return buildFail("用户已被禁用");
		}
		
		// 查询用户代理
		Map<String, Object> map = new HashMap<>(1);
		map.put("userId", user.getId());
		Proxy proxy = proxyService.getUserProxy(map);
		if (null != proxy && proxy.getIsDelete()) {
			logger.error("用户不在是代理：用户名【{}】，密码【{}】，代理【{}】", username, password, proxy);
			return buildFail("用户已被禁用");
		}
		
		// 添加session
		UserInfo userInfo = new UserInfo();
		userInfo.setUserId(user.getId());
		if (null != proxy) {
			userInfo.setProxyId(proxy.getId());
			userInfo.setParentProxyId(proxy.getParentId());
			userInfo.setName(proxy.getWechatName());
		}
		session.setAttribute(Constant.SESSION_KEY, userInfo);
		return buildSuccess("登录成功");
	}
	
	/**
	 * 注册页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/user", method = {RequestMethod.GET})
	public String user(Model model) {
		return "system/register";
	}
	
	/**
	 * 用户注册
	 * 
	 * @param username
	 * @param password
	 * @param code
	 * @return
	 */
	@RequestMapping(value = "/register", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> register(String username, String password, String code) {
		if (StringUtils.isAnyEmpty(username, password, code)) {
			logger.error("注册失败：用户名或密码或验证码为空");
			throw new WmallException("注册失败");
		}
		
		Map<String, Object> map = new HashMap<>(1);
		map.put("code", code);
		VerifyCode verifyCode = verifyCodeService.getVerifyCode(map);
		if (null == verifyCode) {
			logger.error("临时验证码【{}】不存在", code);
			return buildFail("验证码不存在");
		}
		if (verifyCode.getUseStatus()) {
			logger.error("临时验证码【{}】已使用", code);
			return buildFail("验证码已使用");
		}
		if (verifyCode.getEffectiveTime().before(new Date())) {
			logger.error("临时验证码【{}】已过期", code);
			return buildFail("验证码已过期");
		}
		
		// 查询用户
		User existUser = userService.getUserByUsername(username);
		if (null != existUser) {
			logger.error("用户名【{}】已存在", username);
			return buildFail("用户名已存在");
		}
		
		User user = new User();
		user.setUsername(username);
		user.setPassword(Md5Utils.md5(password));
		user.setDisabled(TrueFalseStatusEnum.FALSE.getValue());
		user.setCreateTime(new Date());
		user.setUpdateTime(new Date());
		user.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		user.setProxyId(verifyCode.getProxyId());
		user.setVerifyCode(verifyCode);
		userService.save(user);
		logger.info("【{}】注册成功", user);
		return buildSuccess("注册成功");
	}
	
	/**
	 * 主页
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/main", method = {RequestMethod.GET})
	public String main(Model model) {
		UserInfo userInfo = (UserInfo) session.getAttribute(Constant.SESSION_KEY);
		// 用户权限菜单
		List<Menu> menus = userInfo.getMenus();
		if (CollectionUtils.isEmpty(menus)) {
			// 根据用户查询权限菜单
			menus = menuService.listMenuByUser(userInfo.getUserId());
			userInfo.setMenus(menus);
		}
		model.addAttribute("name", userInfo.getName());
		model.addAttribute("menus", menus);
		model.addAttribute("time", DateUtils.format(new Date(), DateUtils.NORM_DATE_PATTERN));
		model.addAttribute("week", DateUtils.getWeek());
		return "system/main";
	}
	
	/**
	 * 退出页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/logout", method = {RequestMethod.GET})
	public String logout(Model model) {
		// 清除用户session
		session.removeAttribute(Constant.SESSION_KEY);
		return "redirect:/";
	}
	
}
