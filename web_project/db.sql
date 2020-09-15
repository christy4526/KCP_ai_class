CREATE TABLE "User" (
	"user_pid"	int		NOT NULL,
	"user_id"	varchar2(20)		NULL,
	"user_pw"	varchar2(20)		NULL,
	"user_ph"	varchar(15)		NULL,
	"user_name"	varchar2(10)		NULL,
	"user_addr"	varchar2(100)		NULL
);

CREATE TABLE "Board" (
	"board_pid"	int		NOT NULL,
	"board_title"	varchar2(100)		NULL,
	"board_cont"	text		NULL,
	"board_date"	datetime		NULL,
	"board_type"	varchar2(20)		NULL,
	"user_pid"	int		NOT NULL,
	"board_addr"	varchar2(100)		NULL,
	"board_UseYn"	char(1)		NULL
);

CREATE TABLE "Comment" (
	"cmt_pid"	int		NOT NULL,
	"board_pid"	int		NOT NULL,
	"cmt_cont"	text		NULL,
	"user_pid"	int		NOT NULL
);

ALTER TABLE "User" ADD CONSTRAINT "PK_USER" PRIMARY KEY (
	"user_pid"
);

ALTER TABLE "Board" ADD CONSTRAINT "PK_BOARD" PRIMARY KEY (
	"board_pid"
);

ALTER TABLE "Comment" ADD CONSTRAINT "PK_COMMENT" PRIMARY KEY (
	"cmt_pid"
);

ALTER TABLE "Board" ADD CONSTRAINT "FK_USER_TO_BOARD" FOREIGN KEY (
	"user_pid"
)
REFERENCES "User" (
	"user_pid"
);

ALTER TABLE "Comment" ADD CONSTRAINT "FK_USER_TO_Comment" FOREIGN KEY (
	"user_pid"
)
REFERENCES "User" (
	"user_pid"
);

ALTER TABLE "Comment" ADD CONSTRAINT "FK_Board_TO_Comment" FOREIGN KEY (
	"board_pid"
)
REFERENCES "Board" (
	"board_pid"
);

