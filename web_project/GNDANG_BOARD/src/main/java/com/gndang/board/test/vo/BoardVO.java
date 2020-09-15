package com.gndang.board.test.vo;

import java.util.Date;

public class BoardVO {
	private Integer boardPid;
	private String boardTitle;
	private String boardCont;
	private Date boardDate;
	private String boardType;
	private String boardAddr;
	private Character boardUseYn;
	
	private Integer userPid;
	
	public Integer getBoardPid() {
		return boardPid;
	}
	public void setBoardPid(Integer boardPid) {
		this.boardPid = boardPid;
	}
	public String getBoardTitle() {
		return boardTitle;
	}
	public void setBoardTitle(String boardTitle) {
		this.boardTitle = boardTitle;
	}
	public String getBoardCont() {
		return boardCont;
	}
	public void setBoardCont(String boardCont) {
		this.boardCont = boardCont;
	}
	public Date getBoardDate() {
		return boardDate;
	}
	public void setBoardDate(Date boardDate) {
		this.boardDate = boardDate;
	}
	public String getBoardType() {
		return boardType;
	}
	public void setBoardType(String boardType) {
		this.boardType = boardType;
	}
	public String getBoardAddr() {
		return boardAddr;
	}
	public void setBoardAddr(String boardAddr) {
		this.boardAddr = boardAddr;
	}
	public Character getBoardUseYn() {
		return boardUseYn;
	}
	public void setBoardUseYn(Character boardUseYn) {
		this.boardUseYn = boardUseYn;
	}
}
