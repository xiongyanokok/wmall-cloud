package com.xy.wmall.controller;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFCellStyle;
import org.apache.poi.hssf.usermodel.HSSFFont;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.WmallCache;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.DateUtils;
import com.xy.wmall.common.utils.JacksonUtils;
import com.xy.wmall.enums.DeliverTypeEnum;
import com.xy.wmall.enums.FlowStatusEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.Deliver;
import com.xy.wmall.model.DeliverDetail;
import com.xy.wmall.model.DeliverFlow;
import com.xy.wmall.model.Logistics;
import com.xy.wmall.model.Product;
import com.xy.wmall.model.Proxy;
import com.xy.wmall.service.DeliverDetailService;
import com.xy.wmall.service.DeliverFlowService;
import com.xy.wmall.service.DeliverService;
import com.xy.wmall.service.LogisticsService;
import com.xy.wmall.service.ProductService;
import com.xy.wmall.service.ProxyService;

/**
 * Controller
 * 
 * @author admin
 * @date 2017年10月28日 上午08:53:59
 */
@Controller
@RequestMapping(value = "/admin/deliver", produces = { "application/json; charset=UTF-8" })
public class DeliverController extends BaseController {

	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(DeliverController.class);

    @Autowired
	private DeliverService deliverService;
    
    @Autowired
    private ProxyService proxyService;
    
    @Autowired
    private ProductService productService;
    
    @Autowired
    private DeliverDetailService deliverDetailService;
    
    @Autowired
    private LogisticsService logisticsService;
    
    @Autowired
    private DeliverFlowService deliverFlowService;
    
	
	/**
	 * 进入列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/list", method = { RequestMethod.GET })
	public String list(Model model) {
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		return "deliver/list";
	}
	
	/**
	 * 进入我的发货列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/my_list", method = { RequestMethod.GET })
	public String selfList(Model model) {
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		model.addAttribute("deliverType", DeliverTypeEnum.MY_DELIVER.getValue());
		model.addAttribute("proxyId", getProxyId());
		return "deliver/my_list";
	}
	
	/**
	 * 进入代理发货列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/proxy_list", method = { RequestMethod.GET })
	public String proxyList(Model model) {
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		model.addAttribute("deliverType", DeliverTypeEnum.PROXY_DELIVER.getValue());
		return "deliver/proxy_list";
	}
	
	/**
	 * 进入零售发货列表页面
	 * 
	 * @return
	 */
	@RequestMapping(value = "/retail_list", method = { RequestMethod.GET })
	public String retailList(Model model) {
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		model.addAttribute("productsJson", JacksonUtils.serialize(products));
		model.addAttribute("deliverType", DeliverTypeEnum.RETAIL_DELIVER.getValue());
		return "deliver/retail_list";
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
			// 代理id
			map.put("proxyId", request.getParameter("proxyId"));
			// 上级代理id
			map.put("parentProxyId", getProxyId()); 
			// 发货类型
			String deliverType = request.getParameter("deliverType");
			if (StringUtils.isNotEmpty(deliverType)) {
				map.put("proxyId", getProxyId()); 
				if (DeliverTypeEnum.MY_DELIVER.getValue().equals(deliverType)) {
					map.put("parentProxyId", getParentProxyId());
				} else if (DeliverTypeEnum.PROXY_DELIVER.getValue().equals(deliverType)) {
					map.put("deliverType", deliverType); 
				}
			}
			// 产品id
			map.put("productId", request.getParameter("productId")); 
			// 收件人姓名
			map.put("receiveName", request.getParameter("receiveName"));
			// 发货状态
			map.put("deliverStatus", request.getParameter("deliverStatus")); 
			// 发货开始时间
			map.put("startDate", request.getParameter("startDate"));
			// 发货结束时间
			map.put("endDate", request.getParameter("endDate")); 
			// 发货单id
			map.put("groupBy", "id"); 
			// 查询发货单
			List<Deliver> delivers = deliverService.listDeliver(map);
			if (CollectionUtils.isEmpty(delivers)) {
				return Collections.emptyList();
			}
			
			// 获取发货单id
			List<Integer> deliverIds = new ArrayList<>(delivers.size());
			for (Deliver deliver : delivers) {
				deliverIds.add(deliver.getId());
			}
			// 查询发货单详情
			Map<String, Object> detailMap = new HashMap<>(1);
			detailMap.put("deliverIds", deliverIds);
			List<DeliverDetail> deliverDetails = deliverDetailService.listDeliverDetail(detailMap);
			for (Deliver deliver : delivers) {
				List<DeliverDetail> details = new ArrayList<>();
				for (DeliverDetail deliverDetail : deliverDetails) {
					if (deliver.getId().equals(deliverDetail.getDeliverId())) {
						details.add(deliverDetail);
					}
				}
				deliver.setDeliverDetails(details);
			}
			return delivers;
		});
	}
	
	/**
	 * 进入新增页面
	 * 
	 * @param model
	 * @return
	 */
	@RequestMapping(value = "/add", method = { RequestMethod.GET })
	public String add(Model model, Integer proxyId) {
		Assert.notNull(proxyId, "proxyId为空");
		Proxy proxy = proxyService.getProxyById(proxyId);
		Assert.notNull(proxy, "代理不存在");
		model.addAttribute("proxy", proxy);
		if (proxyId != getProxyId()) {
			model.addAttribute("proxyId", proxyId);
		}
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		return "deliver/add";
	}
	
	/**
	 * 保存数据
	 * 
	 * @param deliver
	 * @return
	 */
	@RequestMapping(value = "/save", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> save(Deliver deliver) {
		Assert.notNull(deliver, "保存数据为空");
		deliver.setParentProxyId(getParentProxyId());
		deliver.setCreateUserId(getUserId());
		deliver.setCreateTime(new Date());
		deliver.setUpdateUserId(getUserId());
		deliver.setUpdateTime(new Date());
		deliver.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
		deliverService.save(deliver);
		logger.info("【{}】保存成功", deliver);
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
		Deliver deliver = deliverService.getDeliverById(id);
		Assert.notNull(deliver, id + "不存在");
		model.addAttribute("deliver", deliver);
		// 查询发货详情
		Map<String, Object> map = new HashMap<>(1);
		map.put("deliverId", deliver.getId());
		List<DeliverDetail> deliverDetails = deliverDetailService.listDeliverDetail(map);
		model.addAttribute("deliverDetails", deliverDetails);
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		return "deliver/edit";
	}
	
	/**
	 * 修改数据
	 * 
	 * @param deliver
	 * @return
	 */
	@RequestMapping(value = "/update", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> update(Deliver deliver) {
		Assert.notNull(deliver, "修改数据为空");
		Deliver deliverInfo = deliverService.getDeliverById(deliver.getId());
		Assert.notNull(deliverInfo, "数据不存在");
		deliver.setUpdateUserId(getUserId());
		deliver.setUpdateTime(new Date());
		deliverService.update(deliver);
		logger.info("【{}】修改成功", deliver);
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
		Deliver deliver = deliverService.getDeliverById(id);
		Assert.notNull(deliver, "数据不存在");
		deliverService.remove(deliver);
		logger.info("【{}】删除成功", deliver);
		return buildSuccess("删除成功");
	}
	
	/**
	 * 上报发货
	 * 
	 * @param flowId
	 * @return
	 */
	@RequestMapping(value = "/report", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> report(Integer flowId) {
		Assert.notNull(flowId, "flowId为空");
		DeliverFlow deliverFlow = deliverFlowService.getDeliverFlowById(flowId);
		Assert.notNull(deliverFlow, "数据不存在");
		
		if (!FlowStatusEnum.START.getValue().equals(deliverFlow.getFlowStatus())) {
			deliverFlow.setFlowStatus(FlowStatusEnum.REPORT.getValue());
			deliverFlowService.update(deliverFlow);
		}
		
		DeliverFlow flow = new DeliverFlow();
		flow.setDeliverId(deliverFlow.getDeliverId());
		flow.setProxyId(deliverFlow.getParentProxyId());
		flow.setParentProxyId(getParentProxyId());
		flow.setFlowStatus(FlowStatusEnum.HANDLE.getValue());
		flow.setCreateTime(new Date());
		deliverFlowService.save(flow);
		logger.info("【{}】上报成功", flow);
		return buildSuccess("上报成功");
	}
	
	/**
	 * 撤销发货
	 * 
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/reset", method = { RequestMethod.POST })
	@ResponseBody
	public Map<String, Object> deliverReset(Integer id) {
		Assert.notNull(id, "id为空");
		Deliver deliverInfo = deliverService.getDeliverById(id);
		Assert.notNull(deliverInfo, "数据不存在");
		// 撤销发货
		Deliver deliver = new Deliver();
		deliver.setId(id);
		deliver.setDeliverStatus(TrueFalseStatusEnum.FALSE.getValue());
		deliver.setUpdateUserId(getUserId());
		deliver.setUpdateTime(new Date());
		deliverService.status(deliver);
		logger.info("【{}】撤销发货成功", deliver);
		return buildSuccess("撤销发货成功");
	}
	
	/**
	 * 进入详情页面
	 * 
	 * @param model
	 * @param id
	 * @return
	 */
	@RequestMapping(value = "/detail", method = { RequestMethod.GET })
	public String detail(Model model, Integer id) {
		Assert.notNull(id, "id为空");
		// 查询发货信息
		Deliver deliver = deliverService.getDeliverById(id);
		Assert.notNull(deliver, "数据不存在");
		model.addAttribute("deliver", deliver);
		
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		map.put("deliverId", id);
		// 查询发货详情
		List<DeliverDetail> deliverDetails = deliverDetailService.listDeliverDetail(map);
		model.addAttribute("deliverDetails", deliverDetails);
		
		// 产品列表
		List<Product> products = productService.listProduct();
		model.addAttribute("products", products);
		
		// 发货物流信息
		Logistics logistics = logisticsService.getLogistics(map);
		if (null != logistics) {
			logistics.setName(WmallCache.getLogisticsCompanyName(logistics.getCompanyId()));
			model.addAttribute("logistics", logistics);
		}
		return "deliver/detail";
	}
	
	/**
	 * 导出excel
	 */
	@RequestMapping(value = "/export", method = { RequestMethod.GET })
	public void export() {
		Map<String, Object> map = CommonUtils.defaultQueryMap();
		// 代理id
		String proxyId = request.getParameter("proxyId");
		map.put("proxyId", proxyId); 
		// 产品id
		map.put("productId", request.getParameter("productId")); 
		// 收件人姓名
		map.put("receiveName", request.getParameter("receiveName"));
		// 发货状态
		map.put("deliverStatus", request.getParameter("deliverStatus")); 
		// 发货开始时间
		map.put("startDate", request.getParameter("startDate"));
		// 发货结束时间
		map.put("endDate", request.getParameter("endDate"));
		// 发货单id
		map.put("groupBy", "id");
		// 发货时间排序
		map.put("orderBy", "create_time desc");
		List<Deliver> delivers = deliverService.listDeliver(map);
		if (CollectionUtils.isEmpty(delivers)) {
			return;
		}

		// 获取发货单id
		List<Integer> deliverIds = new ArrayList<>(delivers.size());
		for (Deliver deliver : delivers) {
			deliverIds.add(deliver.getId());
		}
		// 查询发货单详情
		Map<String, Object> detailMap = new HashMap<>(1);
		detailMap.put("deliverIds", deliverIds);
		List<DeliverDetail> deliverDetails = deliverDetailService.listDeliverDetail(detailMap);
		// 查询快递信息
		List<Logistics> logisticss = logisticsService.listLogistics(detailMap);
		for (Deliver deliver : delivers) {
			List<DeliverDetail> details = new ArrayList<>();
			for (DeliverDetail deliverDetail : deliverDetails) {
				if (deliver.getId().equals(deliverDetail.getDeliverId())) {
					details.add(deliverDetail);
				}
			}
			deliver.setDeliverDetails(details);
			for (Logistics logistics : logisticss) {
				if (deliver.getId().equals(logistics.getDeliverId())) {
					deliver.setLogistics(logistics);
					break;
				}
			}
		}

		String wechatName = "";
		if (StringUtils.isNotEmpty(proxyId)) {
			Proxy proxy = proxyService.getProxyById(Integer.valueOf(proxyId));
			Assert.notNull(proxy, "代理不存在");
			wechatName = proxy.getWechatName() + "_";
		}

		// 导出excel
		exportExcel(wechatName, delivers);
	}
	
	/**
	 * 表头
	 * 
	 * @param workbook
	 * @param sheet
	 * @param deliverType
	 */
	private void createTitle(HSSFWorkbook workbook, HSSFSheet sheet) {
		// 表头
		HSSFRow row = sheet.createRow(0);
		// 设置列宽，setColumnWidth的第二个参数要乘以256，这个参数的单位是1/256个字符宽度
		sheet.setColumnWidth(0, 15 * 256);
		sheet.setColumnWidth(1, 15 * 256);
		sheet.setColumnWidth(2, 60 * 256);
		sheet.setColumnWidth(3, 12 * 256);
		sheet.setColumnWidth(4, 50 * 256);
		sheet.setColumnWidth(5, 10 * 256);
		sheet.setColumnWidth(6, 12 * 256);
		sheet.setColumnWidth(7, 16 * 256);
		sheet.setColumnWidth(8, 10 * 256);

		// 设置为居中加粗
		HSSFCellStyle style = workbook.createCellStyle();
		// 居中
		style.setAlignment(HorizontalAlignment.CENTER);
		// 下边框
		style.setBorderBottom(BorderStyle.THIN);
		// 左边框
		style.setBorderLeft(BorderStyle.THIN);
		// 上边框
		style.setBorderTop(BorderStyle.THIN);
		// 右边框
		style.setBorderRight(BorderStyle.THIN);
		// 字体设置
		HSSFFont font = workbook.createFont();
		style.setFont(font);
		// 字体
		font.setFontName("宋体");
		// 字体大小
		font.setFontHeightInPoints((short) 12);
		// 加粗
		font.setBold(true);

		HSSFCell cell;
		cell = row.createCell(0);
		cell.setCellValue("收件人姓名");
		cell.setCellStyle(style);
	
		cell = row.createCell(1);
		cell.setCellValue("收件人电话");
		cell.setCellStyle(style);
	
		cell = row.createCell(2);
		cell.setCellValue("收件人地址");
		cell.setCellStyle(style);
		
		cell = row.createCell(3);
		cell.setCellValue("发货时间");
		cell.setCellStyle(style);
		
		cell = row.createCell(4);
		cell.setCellValue("产品明细");
		cell.setCellStyle(style);
		
		cell = row.createCell(5);
		cell.setCellValue("快递费");
		cell.setCellStyle(style);
		
		cell = row.createCell(6);
		cell.setCellValue("物流公司");
		cell.setCellStyle(style);
		
		cell = row.createCell(7);
		cell.setCellValue("物流单号");
		cell.setCellStyle(style);
		
		cell = row.createCell(8);
		cell.setCellValue("物流价格");
		cell.setCellStyle(style);
	}

	/**
	 * 导出excel
	 * 
	 * @param wechatName
	 * @param delivers
	 */
	public void exportExcel(String wechatName, List<Deliver> delivers) {
		HSSFWorkbook workbook = new HSSFWorkbook();
		// 页签
		HSSFSheet sheet = workbook.createSheet("发货明细");
		// 表头
		createTitle(workbook, sheet);
		
		// 产品信息
		List<Product> products = productService.listProduct();
		Map<Integer, String> productMap = new HashMap<>(products.size());
		for (Product product : products) {
			productMap.put(product.getId(), product.getProductName());
		}
		
		// 新增数据行，并且设置单元格数据
		int rowNum = 1;
		Map<Integer, AtomicInteger> productCountMap = new HashMap<>();
		for (Deliver deliver : delivers) {
			HSSFRow row = sheet.createRow(rowNum++);
			row.createCell(0).setCellValue(deliver.getReceiveName());
			row.createCell(1).setCellValue(deliver.getReceivePhone());
			row.createCell(2).setCellValue(deliver.getReceiveAddress());
			row.createCell(3).setCellValue(DateUtils.format(deliver.getCreateTime(), DateUtils.NORM_DATE_PATTERN));
			List<DeliverDetail> deliverDetails = deliver.getDeliverDetails();
			if (null != deliverDetails) {
				StringBuilder details = new StringBuilder();
				for (DeliverDetail deliverDetail : deliverDetails) {
					if (details.length() > 0) {
						details.append("，");
					}
					details.append(productMap.get(deliverDetail.getProductId()) + "(" + deliverDetail.getAmount() + ")");
					AtomicInteger productCount = productCountMap.get(deliverDetail.getProductId());
					if (null == productCount) {
						productCount = new AtomicInteger(0);
						productCountMap.put(deliverDetail.getProductId(), productCount);
					}
					productCount.addAndGet(deliverDetail.getAmount());
				}
				row.createCell(4).setCellValue(details.toString());
			}
			if (deliver.getCourierPrice() > 0) {
				row.createCell(5).setCellValue(deliver.getCourierPrice());
			}
			Logistics logistics = deliver.getLogistics(); 
			if (null != logistics) {
				row.createCell(6).setCellValue(WmallCache.getLogisticsCompanyName(logistics.getCompanyId()));
				row.createCell(7).setCellValue(logistics.getNumber());
				row.createCell(8).setCellValue(logistics.getPrice() + logistics.getCost());
			}
		}
		
		rowNum++;
		for (Map.Entry<Integer, AtomicInteger> entry : productCountMap.entrySet())  {
			HSSFRow row = sheet.createRow(rowNum++);
			row.createCell(0).setCellValue(productMap.get(entry.getKey()));
			row.createCell(1).setCellValue(entry.getValue().get());
		}
		
		try (OutputStream out = response.getOutputStream();) {
			String fileName = wechatName + "发货明细_" + DateUtils.format(new Date(), "yyyyMMdd") + ".xls";
			response.setContentType("application/octet-stream");
			response.setHeader("Connection", "close");
			response.setHeader("name", fileName);
			response.setHeader("Cache-Control", "must-revalidate, post-check=0, pre-check=0");
			response.setHeader("Pragma", "public");
			response.setDateHeader("Expires", 0);
			response.setHeader("Content-Disposition", "attachment;filename=" + new String(fileName.getBytes("gb2312"), "ISO8859-1"));
			// 输出
			workbook.write(out);
		} catch (Exception e) {
			logger.error("导出excel失败：", e);
		}
	}
}
