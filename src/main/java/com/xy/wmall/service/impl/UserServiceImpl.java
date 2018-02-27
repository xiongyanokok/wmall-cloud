package com.xy.wmall.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.Constant;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.UserMapper;
import com.xy.wmall.mapper.UserProxyMapper;
import com.xy.wmall.mapper.UserRoleMapper;
import com.xy.wmall.mapper.VerifyCodeMapper;
import com.xy.wmall.model.User;
import com.xy.wmall.model.UserProxy;
import com.xy.wmall.model.UserRole;
import com.xy.wmall.model.VerifyCode;
import com.xy.wmall.service.UserService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:39
 */
@Service
public class UserServiceImpl extends BaseServiceImpl<UserMapper, User> implements UserService {

    @Autowired
	private UserMapper userMapper;
    
    @Autowired
    private UserProxyMapper userProxyMapper;
    
    @Autowired
    private UserRoleMapper userRoleMapper;
    
    @Autowired
    private VerifyCodeMapper verifyCodeMapper;
	
	/**
     * 保存数据
     *
     * @param user
     * @throws WmallException
     */
    @Override
    public void save(User user) {
    	Assert.notNull(user, "保存数据为空");
    	try {
    		// 保存用户
			userMapper.insert(user);
			
			// 保存用户代理
			UserProxy userProxy = new UserProxy();
			userProxy.setUserId(user.getId());
			userProxy.setProxyId(user.getProxyId());
			userProxyMapper.insert(userProxy);
			
			// 保存用户角色
			UserRole userRole = new UserRole();
			userRole.setUserId(user.getId());
			userRole.setRoleId(Constant.PROXY_ROLE);
			userRoleMapper.insert(userRole);
			
			// 更新验证码
			VerifyCode verifyCode = user.getVerifyCode();
			verifyCode.setUseStatus(TrueFalseStatusEnum.TRUE.getValue());
			verifyCodeMapper.update(verifyCode);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + user.toString() + "】保存失败", e);
		}
    }

    /**
     * 根据用户
     * 
     * @param username
     * @return
     */
    @Override
    public User getUserByUsername(String username) {
    	Assert.hasLength(username, "username为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("username", username);
	    	return userMapper.getByMap(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + username + "】根据用户失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    @Override
    public List<User> listUserRole(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return userMapper.listUserRole(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询列表失败", e);
		}
    }
    
}
