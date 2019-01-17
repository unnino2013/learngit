package com.zdd.risk.utils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.Random;

/**
 * 工具类
 * @author 租无忧科技有限公司
 * @date 2018-11-01.
 */
public class Tools {
    private static final Random random = new Random();

    public static void copyFileUsingFileChannels(File source, File dest) throws IOException {
        FileChannel inputChannel = null;
        FileChannel outputChannel = null;
        try {
            inputChannel = new FileInputStream(source).getChannel();
            outputChannel = new FileOutputStream(dest).getChannel();
            outputChannel.transferFrom(inputChannel, 0, inputChannel.size());
        } finally {
            assert inputChannel != null;
            inputChannel.close();
            assert outputChannel != null;
            outputChannel.close();
        }
    }

    public static int rand(int min, int max) {
        return random.nextInt(max) % (max - min + 1) + min;
    }

	public static String flowAutoShow(double value) {
		// Math.round 方法接收 float 和 double 类型,如果参数是 int 的话,会强转为 float,这个时候调用该方法无意义
        int kb = 1024;
        int mb = 1048576;
        int gb = 1073741824;
		double abs = Math.abs(value);
		if (abs > gb) {
            return Math.round(value / gb) + "GB";
		} else if (abs > mb) {
            return Math.round(value / mb) + "MB";
		} else if (abs > kb) {
            return Math.round(value / kb) + "KB";
        }
        return Math.round(value) + "";
    }

    public static String enAes(String data, String key) throws Exception {
        Cipher cipher = Cipher.getInstance("AES");
        SecretKeySpec secretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
        cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
        byte[] encryptedBytes = cipher.doFinal(data.getBytes());
        return new BASE64Encoder().encode(encryptedBytes);
    }

    public static String deAes(String data, String key) throws Exception {
        Cipher cipher = Cipher.getInstance("AES");
        SecretKeySpec secretKeySpec = new SecretKeySpec(key.getBytes("UTF-8"), "AES");
        cipher.init(Cipher.DECRYPT_MODE, secretKeySpec);
        byte[] cipherTextBytes = new BASE64Decoder().decodeBuffer(data);
        byte[] decValue = cipher.doFinal(cipherTextBytes);
        return new String(decValue);
    }

    /**
     * 判断字符串是否为数字和有正确的值
     *
     * @param str
     * @return
     */
    public static boolean isNumber(String str) {
        // Pattern pattern=Pattern.compile("[0-9]*");
        // return pattern.matcher(str).matches();
        if (null != str && 0 != str.trim().length() && str.matches("\\d*")) {
            return true;
        }

        return false;
    }
    public static void main(String [] args){
        String result1="[{\"score\":656.0862,\"advice\":null,\"advice_loan_amt\":2000}]";
        JSONObject result = JSON.parseArray(result1).getJSONObject(0);
        String result2="{\"market\": 4000,\"total_deposit\": 800,\"credit_cost\": 60,\"zm_jianmian\": 1000,\"zmscore\": \"Z1( <600 )\",\"orderid\": \"123124124\",\"idcard_no\": \"522627199205170415\",\"realname\": \"陆曙\",\"tel\":\"15801353890\",\"bankcard_no\":\"6464641654\"}";
        result =JSON.parseObject(result2);
        System.out.println(result.getBigDecimal("market"));
    }
}
