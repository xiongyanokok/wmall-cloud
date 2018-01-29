package com.xy.wmall.enums;

/**
 * 流程状态
 * 
 * @author xiongyan
 * @date 2016年11月21日 上午11:39:33
 */
public enum FlowStatusEnum {
	
	/**
	 * 开始
	 */
	START(1),

	/**
	 * 处理
	 */
	HANDLE(2),
	
	/**
	 * 上报
	 */
	REPORT(3),
	
	/**
	 * 完成
	 */
	COMPLETE(4);
	
	/**
	 * 状态
	 */
	private Integer value;
	
	private FlowStatusEnum(Integer value){
		this.value= value;
	}

	public Integer getValue() {
		return value;
	}
}
