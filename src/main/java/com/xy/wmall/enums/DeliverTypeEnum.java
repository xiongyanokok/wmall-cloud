package com.xy.wmall.enums;

/**
 * 发货类型
 * 
 * @author xiongyan
 * @date 2016年11月21日 上午11:39:33
 */
public enum DeliverTypeEnum {
	
	/**
	 * 我的发货
	 */
	MY_DELIVER("1"),

	/**
	 * 代理发货
	 */
	PROXY_DELIVER("2"),
	
	/**
	 * 零售发货
	 */
	RETAIL_DELIVER("3");
	
	/**
	 * 状态
	 */
	private String value;
	
	private DeliverTypeEnum(String value){
		this.value= value;
	}

	public String getValue() {
		return value;
	}
}
