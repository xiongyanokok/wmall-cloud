package com.xy.wmall.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.ListPageUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.VerifyCodeMapper;
import com.xy.wmall.model.VerifyCode;
import com.xy.wmall.service.VerifyCodeService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:45
 */
@Service
public class VerifyCodeServiceImpl implements VerifyCodeService {

    @Autowired
	private VerifyCodeMapper verifyCodeMapper;
	
	/**
     * 保存数据
     *
     * @param verifyCode
     * @throws WmallException
     */
    @Override
    public void save(VerifyCode verifyCode) {
    	Assert.notNull(verifyCode, "保存数据为空");
    	try {
			verifyCodeMapper.insert(verifyCode);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + verifyCode.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param verifyCode
     * @throws WmallException
     */
    @Override
    public void update(VerifyCode verifyCode) {
    	Assert.notNull(verifyCode, "修改数据为空");
    	try {
    		verifyCodeMapper.update(verifyCode);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + verifyCode.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param verifyCode
     * @throws WmallException
     */
    @Override
    public void remove(VerifyCode verifyCode) {
    	Assert.notNull(verifyCode, "删除数据为空");
		try {
    		VerifyCode deleteVerifyCode = new VerifyCode();
    		deleteVerifyCode.setId(verifyCode.getId());
    		verifyCodeMapper.update(deleteVerifyCode);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + verifyCode.toString() + "】删除失败", e);
    	}
    }
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public VerifyCode getVerifyCodeById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return verifyCodeMapper.getVerifyCode(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public VerifyCode getVerifyCode(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return verifyCodeMapper.getVerifyCode(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询对象失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public List<VerifyCode> listVerifyCode(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return verifyCodeMapper.listVerifyCode(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询列表失败", e);
		}
    }
    
    /**
     * 批量保存
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchSave(List<VerifyCode> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<VerifyCode>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<VerifyCode> page : pageList) {
				verifyCodeMapper.batchInsert(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量保存失败", e);
		}
    }
    
    /**
     * 批量更新
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchUpdate(List<VerifyCode> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<VerifyCode>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<VerifyCode> page : pageList) {
				verifyCodeMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
