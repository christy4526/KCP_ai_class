package com.gndang.board.test.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gndang.board.test.dao.TestDao;
import com.gndang.board.test.vo.UserVO;


@Service
public class TestService {
	@Autowired
	private TestDao testDao;
	
	// TestVO의 값을 받아오는 함수를 TestDao에서 호출하도록 하는 함수
	// private TestDao testDao = new TestDao();
	// component를 bean이라고 함
	public UserVO getTest(int key) {
		return this.testDao.getTest(key);
	}
	
	public List<UserVO> getTests() {
		return this.testDao.getTests();
	}
}
