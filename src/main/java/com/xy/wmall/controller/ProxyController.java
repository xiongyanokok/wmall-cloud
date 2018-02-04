package com.xy.wmall.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.Constant;
import com.xy.wmall.common.utils.DateUtils;
import com.xy.wmall.common.utils.JacksonUtils;
import com.xy.wmall.common.utils.Md5Utils;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Product;
import com.xy.wmall.model.Proxy;
import com.xy.wmall.model.VerifyCode;
import com.xy.wmall.service.ProductService;
import com.xy.wmall.service.ProxyService;
import com.xy.wmall.service.UserProxyService;
import com.xy.wmall.service.VerifyCodeService;
import com.xy.wmall.service.WalletService;

/**
 * Controller
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:21
 */
@Controller
@RequestMapping(value = "/admin/proxy", produces = { "application/json; charset=UTF-8" })
public class ProxyController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(ProxyController.class);

    @Autowired
	private ProxyService proxyService;
    
    @Autowired
    private ProductService productService;
    
    @Autowired
    private WalletService walletService;
    
    @Autowired
    private VerifyCodeService verifyCodeService;
    
    @Autowired
    private UserProxyService userProxyService;
    
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		return "proxy/list";
	}
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/bill", method = { RequestMethod.GET })
	public String bill(Model model) {
		return "proxy/bill_list";
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
			// 上级代理id
			map.put("parentId", getProxyId()); 
			// 微信昵称
			map.put("wechatName", request.getParameter("wechatName"));
			// 姓名
			map.put("name", request.getParameter("name")); 
			// 手机号
			map.put("phone", request.getParameter("phone"));
			// 查询代理
			return proxyService.listProxy(map);
		});
	}
	
	/**
	 * 列表分页查询
	 * 
	 * @return
	 */
	@RequestMapping(value = "/bill_query", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> billQuery() {
		return pageInfoResult(map -> {
			// 查询条件
			// 上级代理id
			map.put("parentId", getProxyId()); 
			// 微信昵称
			map.put("wechatName", request.getParameter("wechatName"));
			// 姓名
			map.put("name", request.getParameter("name")); 
			// 查询代理
			List<Proxy> proxyList = proxyService.listProxy(map);
			if (CollectionUtils.isEmpty(proxyList)) {
				return proxyList;
			}
			// 代理id
			List<Integer> proxyIds = new ArrayList<>();
			for (Proxy proxy : proxyList) {
				proxyIds.add(proxy.getId());
			}
			// 批量查询代理钱包余额
			Map<Integer, Integer> balanceMap = walletService.listWalletBalance(proxyIds);
			// 批量查询代理用户
			Map<Integer, Integer> userProxyMap = userProxyService.listUserByProxy(proxyIds);
			for (Proxy proxy : proxyList) {
				proxy.setBalance(balanceMap.get(proxy.getId()));
				proxy.setOpenUser(userProxyMap.containsKey(proxy.getId()));
			}
			return proxyList;
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
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		return "proxy/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param proxy
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Proxy proxy) {
		Assert.notNull(proxy, "保存数据为空");
		proxy.setParentId(getProxyId());
		proxy.setCreateUserId(getUserId());
		proxy.setCreateTime(new Date());
		proxy.setUpdateUserId(getUserId());
		proxy.setUpdateTime(new Date());
		proxy.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		proxyService.save(proxy);
		logger.info("【{}】保存成功", proxy);
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
		Proxy proxy = proxyService.getProxyById(id);
		Assert.notNull(proxy, "数据不存在");
		model.addAttribute("proxy", proxy);
		return "proxy/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param proxy
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Proxy proxy) {
		Assert.notNull(proxy, "修改数据为空");
		Proxy proxyInfo = proxyService.getProxyById(proxy.getId());
		Assert.notNull(proxyInfo, "数据不存在");
		proxy.setUpdateUserId(getUserId());
		proxy.setUpdateTime(new Date());
		proxyService.update(proxy);
		logger.info("【{}】修改成功", proxy);
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
		Proxy proxy = proxyService.getProxyById(id);
		Assert.notNull(proxy, "数据不存在");
		proxyService.remove(proxy);
		logger.info("【{}】删除成功", proxy);
		return buildSuccess("删除成功");
	}
	
	/**
	 * 代理详情
	 * 
	 * @param model
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/detail", method = {RequestMethod.GET})
	public String detail(Model model, Integer id) {
		Assert.notNull(id, "id为空");
		Proxy proxy = proxyService.getProxyById(id);
		Assert.notNull(proxy, "数据不存在");
		model.addAttribute("proxy", proxy);
		return "proxy/detail";
	}
	
	/**
	 * 进入补货单列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/order_list", method = { RequestMethod.GET })
	public String orderList(Model model, Integer proxyId) {
		Assert.notNull(proxyId, "proxyId为空");
		Proxy proxy = proxyService.getProxyById(proxyId);
		Assert.notNull(proxy, "代理不存在");
		model.addAttribute("proxy", proxy);
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		model.addAttribute("years", DateUtils.listYear());
		model.addAttribute("months", DateUtils.listMonth());
		return "proxy/order_list";
	}
	
	/**
	 * 进入发货单列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/deliver_list", method = { RequestMethod.GET })
	public String deliverList(Model model, Integer proxyId) {
		Assert.notNull(proxyId, "proxyId为空");
		Proxy proxy = proxyService.getProxyById(proxyId);
		Assert.notNull(proxy, "代理不存在");
		model.addAttribute("proxy", proxy);
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		return "proxy/deliver_list";
	}
	
	/**
	 * 生成临时验证码
	 * 
	 * @param proxyId
	 * @return
	 */
	@RequestMapping(value = "/code", method = { RequestMethod.GET })
	public String code(Model model, Integer proxyId) {
		Assert.notNull(proxyId, "proxyId为空");
		Proxy proxy = proxyService.getProxyById(proxyId);
		Assert.notNull(proxy, "数据不存在");
		model.addAttribute("proxy", proxy);
		// 生成临时验证码
		VerifyCode verifyCode = new VerifyCode();
		verifyCode.setProxyId(proxyId);
		String code = Md5Utils.md5(proxyId + getProxyId() + UUID.randomUUID().toString());
		verifyCode.setCode(code);
		Date date = new Date();
		verifyCode.setCreateUserId(getProxyId());
		verifyCode.setCreateTime(date);
		verifyCode.setEffectiveTime(org.apache.commons.lang3.time.DateUtils.addMinutes(date, Constant.CODE_EFFECTIVE_TIME));
		verifyCode.setUseStatus(TrueFalseStatusEnum.FALSE.getValue());
		verifyCodeService.save(verifyCode);
		model.addAttribute("code", code);
		return "proxy/code";
	}
	
}
