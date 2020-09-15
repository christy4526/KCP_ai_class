package com.gndang.board.core.config;

import javax.sql.DataSource;

import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


//설정을 담당하는 annotation으로 쓰겠다
//core안에는 기본 설정들이 들어간다
@Configuration
public class DataSourceConfig {
	/**
	    * DataSource 객체 생성
	    *
	    * @methodName   dataSource
	    * @author      dkdlrja
	    * @return
	    */
	   @Bean(name = "dataSource")
	   @ConfigurationProperties(prefix = "spring.datasource") //properties에서 prefix에 있는 설정값을 가져온다
	   public DataSource dataSource() {
	      return DataSourceBuilder.create().build();
	   }
	   
	   /**
	    * SqlSessionFactory 객체 생성
	    * 
	    * @methodName   sqlSessionFactory
	    * @author      dkdlrja
	    * @param      dataSource
	    * @param      applicationContext
	    * @return
	    * @throws      Exception
	    */
	   @Bean(name = "sqlSessionFactory") //위에서 만든 것에 추가 설정을 붙인다
	   public SqlSessionFactory sqlSessionFactory(@Qualifier("dataSource") DataSource dataSource, ApplicationContext applicationContext) throws Exception {
	      SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
	      sqlSessionFactoryBean.setDataSource(dataSource);
	      sqlSessionFactoryBean.setMapperLocations(applicationContext.getResources("classpath*:mybatis/mapper/**/*.xml"));
	      sqlSessionFactoryBean.setTypeAliasesPackage("**.vo"); //쿼리를 자동으로 VO에 넣어줌 
	      return sqlSessionFactoryBean.getObject();
	   }
	   
	   /**
	    * SqlSessionTemplate 객체 생성
	    * @methodName   sqlSessionTemplate
	    * @author      dkdlrja
	    * @param      sqlSessionFactory
	    * @return
	    * @throws      Exception
	    */
	   @Bean(name = "sqlSessionTemplate") //sql 이름을 설정
	   public SqlSessionTemplate sqlSessionTemplate(SqlSessionFactory sqlSessionFactory) throws Exception {
	      return new SqlSessionTemplate(sqlSessionFactory);
	   }
}
