package com.xy.wmall.common;

/**
 * 常量
 * 
 * @author xiongyan
 * @date 2017年10月27日 下午2:09:13
 */
public final class Constant {
	
	private Constant() {
		
	}
	
	/**
	 * 编码
	 */
	public static final String CHARSETNAME = "UTF-8";
	
	/**
	 * 默认错误页面
	 */
	public static final String DEFAULT_ERROR_VIEW = "error";
	
	/**
	 * 状态码
	 */
	public static final String DEFAULT_CODE = "code";
	
	/**
	 * 描述信息
	 */
	public static final String DEFAULT_MESSAGE = "message";
	
	/**
	 * 响应数据
	 */
	public static final String DEFAULT_DATA = "data";
	
	/**
	 * 删除标记
	 */
	public static final String ISDELETE = "isDelete";

	/**
	 * session key
	 */
	public static final String SESSION_KEY = "session_user";
	
	/**
	 * image code
	 */
	public static final String IMAGE_CODE = "image_code";
	
	/**
	 * product cache
	 */
	public static final String PRODUCT_CACHE = "product";
	
	/**
	 * logistics cache
	 */
	public static final String LOGISTICS_CACHE = "logistics";
	
	/**
	 * 验证码有效时间10分钟
	 */
	public static final Integer CODE_EFFECTIVE_TIME = 10;
	
	/**
	 * 默认管理员角色
	 */
	public static final Integer ADMIN_ROLE = 1;
	
	/**
	 * 默认代理角色
	 */
	public static final Integer PROXY_ROLE = 2;
	
	/**
	 * 免费试用30天
	 */
	public static final Integer FREE_30_DAY = 30;
	
	/**
	 * 秘钥KEY
	 */
	public static final String DES_KEY = "xy@hs=xpg%1314*happy$family";
	
}
