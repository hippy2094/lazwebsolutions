create table jdo_demo (
	id serial not null,
	ftstr varchar(30),
	ftbool boolean,
	ftdate timestamp without time zone,
	ftfloat numeric,
	ftint integer,
	constraint pk_jdo_demo
		primary key (id)
);