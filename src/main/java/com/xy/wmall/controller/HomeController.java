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

import com.xy.wmall.common.Constant;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.DateUtils;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Notice;
import com.xy.wmall.model.Product;
import com.xy.wmall.pojo.UserInfo;
import com.xy.wmall.service.BackupService;
import com.xy.wmall.service.DeliverService;
import com.xy.wmall.service.NoticeService;
import com.xy.wmall.service.ProductService;
import com.xy.wmall.service.WalletService;

import lombok.extern.slf4j.Slf4j;

/**
 * Controller
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:27
 */
@Controller
@RequestMapping(value = "/admin/home", produces = { "application/json; charset=UTF-8" })
@Slf4j
public class HomeController extends BaseController {

	@Autowired
	private ProductService productService;
	
	@Autowired
	private WalletService walletService;
	
	@Autowired
	private DeliverService deliverService;
	
	@Autowired
	private BackupService backupService;
	
	@Autowired
	private NoticeService noticeService;
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/index", method = { RequestMethod.GET })
	public String list(Model model) {
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		// 是否管理员
		if (isAdmin()) {
			model.addAttribute("isAdmin", TrueFalseStatusEnum.TRUE.getValue());
			return "home/index";
		}
		
		// 用户代理ID
		Integer proxyId = getProxyId();
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		map.put("operator", "<>");
		map.put("proxyId", proxyId);
		// 代理存款
		Integer proxyWallet = walletService.getStatisticsWallet(map);
		if (null != proxyWallet) {
			model.addAttribute("proxyWallet", proxyWallet);
		}
		// 自己存款
		map.put("operator", "=");
		Integer myWallet = walletService.getStatisticsWallet(map);
		if (null != myWallet) {
			model.addAttribute("myWallet", myWallet);
		}
		// 待发货数量
		int waitDeliver = deliverService.countWaitDeliver(proxyId);
		model.addAttribute("waitDeliver", waitDeliver);
		// 服务有效期剩余天数
		UserInfo userInfo = (UserInfo) session.getAttribute(Constant.SESSION_KEY);
		model.addAttribute("overDay", DateUtils.daysBetween(userInfo.getServiceDate(), new Date()));
		// 最新通知
		Notice notice = noticeService.getNewestNotice();
		model.addAttribute("notice", notice);
		return "home/index";
	}
	
	/**
	 * 数据备份
	 * 
	 * @return
	 */
	@RequestMapping(value = "/backup", method = { RequestMethod.GET })
	@ResponseBody
	public Map<String, Object> backup() {
		if (backupService.backup()) {
			log.info("数据库备份成功");
			return buildSuccess("备份成功");
		} else {
			log.info("数据库备份失败");
			return buildFail("备份失败");
		}
	}
	
}
