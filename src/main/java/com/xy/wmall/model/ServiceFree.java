package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
@Data
public class ServiceFree implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 用户ID
     */
	private Integer userId;
	
	/**
     * 服务费
     */
	private Integer free;
	
	/**
     * 开始时间
     */
	private Date startDate;
	
	/**
     * 结束时间
     */
	private Date endDate;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
}
