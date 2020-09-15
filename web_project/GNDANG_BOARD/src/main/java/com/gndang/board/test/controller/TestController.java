package com.gndang.board.test.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.gndang.board.test.service.TestService;
import com.gndang.board.test.vo.UserVO;


@RestController
@RequestMapping(value = "/danggn")
public class TestController {

	@Autowired
	private TestService testService;
	
	@RequestMapping(value = "/home", method = {RequestMethod.GET})
	public String home() {
		return "Hello World!";
	}
	
	@RequestMapping(value = "/testvo/{key}", method = {RequestMethod.GET})
	public UserVO getTest(@PathVariable(name = "key", required = true) int key) {
		return this.testService.getTest(key);
	}
	
	@RequestMapping(value = "/testvos", method = {RequestMethod.GET})
	public List<UserVO> getTests() {
		return this.testService.getTests();
	}
}
