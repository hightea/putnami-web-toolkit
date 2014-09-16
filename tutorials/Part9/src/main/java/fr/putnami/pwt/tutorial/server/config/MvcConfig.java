package fr.putnami.pwt.tutorial.server.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Import;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

import fr.putnami.pwt.plugin.spring.rpc.server.ComandServiceConfig;
import fr.putnami.pwt.tutorial.server.controller.WelcomeController;
import fr.putnami.pwt.tutorial.server.service.ContactServiceImpl;
import fr.putnami.pwt.tutorial.server.service.IssueServiceImpl;
import fr.putnami.pwt.tutorial.shared.service.ContactService;
import fr.putnami.pwt.tutorial.shared.service.IssueService;

@Configuration
@Import(ComandServiceConfig.class)
@EnableWebMvc
@ComponentScan(basePackages = {"fr.putnami.pwt.tutorial.server.controller", "fr.putnami.pwt.tutorial.server.service"})
public class MvcConfig extends WebMvcConfigurerAdapter {

	private static final int RESOURCE_CACHE_PERIOD = 60 * 60 * 24 * 7; // 1 week in second

	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		registry.addResourceHandler("/index.html").addResourceLocations("/index.html").setCachePeriod(RESOURCE_CACHE_PERIOD);
		registry.addResourceHandler("/Tutorial/**").addResourceLocations("/Tutorial/").setCachePeriod(RESOURCE_CACHE_PERIOD);
	}
}
