package com.gndang.board.test.vo;

public class CommentVO {
	private Integer cmtPid;
	private String cmtCont;
	
	private Integer boardPid;
	private Integer userPid;
	
	public Integer getCmtPid() {
		return cmtPid;
	}
	public void setCmtPid(Integer cmtPid) {
		this.cmtPid = cmtPid;
	}
	public String getCmtCont() {
		return cmtCont;
	}
	public void setCmtCont(String cmtCont) {
		this.cmtCont = cmtCont;
	}
	
}
