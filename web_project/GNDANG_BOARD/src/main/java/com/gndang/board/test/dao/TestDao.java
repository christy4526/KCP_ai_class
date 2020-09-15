package com.gndang.board.test.dao;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.ibatis.session.SqlSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import com.gndang.board.test.vo.UserVO;


@Repository
public class TestDao {
	@Autowired
	@Qualifier("sqlSessionTemplate")
	private SqlSession sqlSession;
	//TestVO의 값들을 받아오는 함수
	//보통 이런식으로 DB에서 값을 받아옴 sqlSession.selectOne("test.test.getTest", param);
	
	public UserVO getTest(int key) {
		Map<String, Object> param = new HashMap<String, Object>();
		param.put("key", key);
		UserVO test = this.sqlSession.selectOne("test.getTest", param);
		return test;
	}
	
	public List<UserVO> getTests() {
		List<UserVO> test = this.sqlSession.selectList("test.getTests");
		return test;
	}
}
