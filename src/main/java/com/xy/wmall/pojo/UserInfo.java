package com.xy.wmall.pojo;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.xy.wmall.model.Menu;

import lombok.Getter;
import lombok.Setter;

/**
 * 用户信息
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:21
 */
@Getter
@Setter
public class UserInfo implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 用户id
	 */
	private Integer userId;
	
	/**
     * 代理ID
     */
	private Integer proxyId;
	
	/**
     * 上级代理ID
     */
	private Integer parentProxyId;
	
	/**
	 * 姓名
	 */
	private String name;
	
	/**
	 * 是否管理员
	 */
	private Boolean isAdmin;
	
	/**
	 * 权限菜单
	 */
	private List<Menu> menus;
	
	/**
     * 服务有效期
     */
	private Date serviceDate;
	
}
