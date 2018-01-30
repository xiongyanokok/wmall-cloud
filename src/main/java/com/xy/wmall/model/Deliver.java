package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2017年11月07日 下午06:03:41
 */
@Data
public class Deliver implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 收件人姓名
     */
	private String receiveName;
	
	/**
     * 收件人电话
     */
	private String receivePhone;
	
	/**
     * 收件人地址
     */
	private String receiveAddress;
	
	/**
     * 快递费
     */
	private Integer courierPrice;
	
	/**
     * 发货状态：0未发货，1已发货
     */
	private Boolean deliverStatus;
	
	/**
     * 授权码
     */
	private String authorizationCode;
	
	/**
     * 备注
     */
	private String remark;
	
	/**
     * 创建人ID
     */
	private Integer createUserId;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
	/**
     * 修改人ID
     */
	private Integer updateUserId;
	
	/**
     * 修改时间
     */
	private Date updateTime;
	
	/**
     * 删除：1删除，0未删除
     */
	private Boolean isDelete;
	
	
	/**
	 * 代理id
	 */
	private Integer proxyId;
	
	/**
	 * 上级代理id
	 */
	private Integer parentProxyId;
	
    /**
     * 产品id
     */
    private Integer[] productId;
    
    /**
     * 数量
     */
    private Integer[] amount;
    
    /**
     * 发货详情
     */
    private List<DeliverDetail> deliverDetails;
    
    /**
     * 发货物流
     */
    private Logistics logistics;
    
    /**
     * 流程id
     */
	private Integer flowId;
	
	/**
     * 流程状态：1开始，2处理，3上报，4完成
     */
	private Integer flowStatus;

}
