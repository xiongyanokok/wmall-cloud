package com.xy.wmall.common.utils;

import java.io.IOException;
import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;

import org.apache.commons.lang3.StringUtils;

import com.xy.wmall.exception.BizException;
import com.xy.wmall.exception.WmallException;

import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

/**
 * 加密解密工具类
 * 
 * @author xiongyan
 * @date 2018年3月2日 上午10:15:22
 */
@SuppressWarnings("restriction")
public class DesUtils {
	
	private static final String DES = "DES";
	
	private DesUtils() {
	}

	/**
	 * 根据键值进行加密
	 * 
	 * @param data
	 * @param key
	 * @return
	 */
	public static String encrypt(String data, String key) {
		if (StringUtils.isEmpty(data)) {
			throw new NullPointerException();
		}
		try {
			byte[] bt = encrypt(data.getBytes(), key.getBytes());
			BASE64Encoder encoder = new BASE64Encoder();
			return encoder.encode(bt);
		} catch (BizException e) {
			throw new WmallException(e);
		}
	}

	/**
	 * 根据键值进行解密
	 * 
	 * @param data
	 * @param key
	 * @return
	 */
	public static String decrypt(String data, String key) {
		if (StringUtils.isEmpty(data)) {
			throw new NullPointerException();
		}
		try {
			BASE64Decoder decoder = new BASE64Decoder();
			byte[] buf = decoder.decodeBuffer(data);
			byte[] bt = decrypt(buf, key.getBytes());
			return new String(bt);
		} catch (IOException | BizException e) {
			throw new WmallException(e);
		}
	}

	/**
	 * 根据键值进行加密
	 * 
	 * @param data
	 * @param key
	 * @return
	 * @throws BizException 
	 */
	private static byte[] encrypt(byte[] data, byte[] key) throws BizException {
		try {
			// 生成一个可信任的随机数源
			SecureRandom sr = new SecureRandom();
			
			// 从原始密钥数据创建DESKeySpec对象
			DESKeySpec dks = new DESKeySpec(key);
			
			// 创建一个密钥工厂，然后用它把DESKeySpec转换成SecretKey对象
			SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(DES);
			SecretKey securekey = keyFactory.generateSecret(dks);
			
			// Cipher对象实际完成加密操作
			Cipher cipher = Cipher.getInstance(DES);
			
			// 用密钥初始化Cipher对象
			cipher.init(Cipher.ENCRYPT_MODE, securekey, sr);
			
			return cipher.doFinal(data);
		} catch (Exception e) {
			throw new BizException(e);
		}
	}

	/**
	 * 根据键值进行解密
	 * 
	 * @param data
	 * @param key
	 * @return
	 * @throws BizException 
	 */
	private static byte[] decrypt(byte[] data, byte[] key) throws BizException {
		try {
			// 生成一个可信任的随机数源
			SecureRandom sr = new SecureRandom();
			
			// 从原始密钥数据创建DESKeySpec对象
			DESKeySpec dks = new DESKeySpec(key);
			
			// 创建一个密钥工厂，然后用它把DESKeySpec转换成SecretKey对象
			SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(DES);
			SecretKey securekey = keyFactory.generateSecret(dks);
			
			// Cipher对象实际完成解密操作
			Cipher cipher = Cipher.getInstance(DES);
			
			// 用密钥初始化Cipher对象
			cipher.init(Cipher.DECRYPT_MODE, securekey, sr);
			
			return cipher.doFinal(data);
		} catch (Exception e) {
			throw new BizException(e);
		}
	}
	
}
