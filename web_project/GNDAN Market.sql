CREATE TABLE "User" (
	"User_pid"	int		NOT NULL,
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
	"User_pid"	int		NOT NULL,
	"board_addr"	varchar2(100)		NULL,
	"board_UseYn"	char(1)		NULL
);

CREATE TABLE "Photo" (
	"pht_id"	int		NOT NULL,
	"pht_name"	varchar2(100)		NULL,
	"board_id"	int		NOT NULL,
	"pht_size"	int		NULL,
	"pht_path"	varchar2(100)		NULL,
	"pht_date"	datetime		NULL,
	"pht_useYn"	char(1)		NULL
);

CREATE TABLE "Used_deal" (
	"board_id"	int		NOT NULL,
	"ud_tag"	varchar2(50)		NULL,
	"ud_price"	int		NULL,
	"ud_dealYn"	varchar2(2)		NULL,
	"ud_state"	varchar2(10)		NULL
);

CREATE TABLE "PR" (
	"board_id"	int		NOT NULL,
	"pr_price"	int		NULL,
	"pr_chatYn"	varchar2(10)		NULL,
	"pr_ph"	varchar2(15)		NULL
);

CREATE TABLE "Comment" (
	"cmt_pid"	int		NOT NULL,
	"board_id"	int		NOT NULL,
	"cmt_cont"	text		NULL,
	"User_pid"	int		NOT NULL
);

ALTER TABLE "User" ADD CONSTRAINT "PK_USER" PRIMARY KEY (
	"User_pid"
);

ALTER TABLE "Board" ADD CONSTRAINT "PK_BOARD" PRIMARY KEY (
	"board_pid"
);

ALTER TABLE "Photo" ADD CONSTRAINT "PK_PHOTO" PRIMARY KEY (
	"pht_id"
);

ALTER TABLE "Used_deal" ADD CONSTRAINT "PK_USED_DEAL" PRIMARY KEY (
	"board_id"
);

ALTER TABLE "PR" ADD CONSTRAINT "PK_PR" PRIMARY KEY (
	"board_id"
);

ALTER TABLE "Comment" ADD CONSTRAINT "PK_COMMENT" PRIMARY KEY (
	"cmt_pid"
);

ALTER TABLE "Used_deal" ADD CONSTRAINT "FK_Board_TO_Used_deal_1" FOREIGN KEY (
	"board_id"
)
REFERENCES "Board" (
	"board_pid"
);

ALTER TABLE "PR" ADD CONSTRAINT "FK_Board_TO_PR_1" FOREIGN KEY (
	"board_id"
)
REFERENCES "Board" (
	"board_pid"
);

