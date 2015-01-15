package fr.putnami.pwt.doc.server.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.filter.CharacterEncodingFilter;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.view.InternalResourceViewResolver;

import fr.putnami.pwt.doc.server.controller.PutnamiController;

@Configuration
@EnableWebMvc
public class MvcConfig extends WebMvcConfigurerAdapter {

	private static final int RESOURCE_CACHE_PERIOD = 60 * 60 * 24 * 7; // 1 week in second

	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		registry.addResourceHandler("favicon.ico")
			.addResourceLocations("/assets/favicon.ico").setCachePeriod(RESOURCE_CACHE_PERIOD);
		registry.addResourceHandler("robots.txt")
			.addResourceLocations("assets/robots.txt").setCachePeriod(RESOURCE_CACHE_PERIOD);
		registry.addResourceHandler("/assets/**")
			.addResourceLocations("/assets/").setCachePeriod(RESOURCE_CACHE_PERIOD);
		registry.addResourceHandler("/Documentation/**")
			.addResourceLocations("/Documentation/").setCachePeriod(RESOURCE_CACHE_PERIOD);

	}

	@Bean
	public InternalResourceViewResolver getInternalResourceViewResolver() {
		InternalResourceViewResolver resolver = new InternalResourceViewResolver();
		resolver.setPrefix("/WEB-INF/pages/");
		resolver.setSuffix(".jsp");
		return resolver;
	}

	@Bean
	public PutnamiController putnamiController() {
		return new PutnamiController();
	}

	@Bean
	public CharacterEncodingFilter characterEncodingFilter() {
		CharacterEncodingFilter characterEncodingFilter = new CharacterEncodingFilter();
		characterEncodingFilter.setEncoding("UTF-8");
		characterEncodingFilter.setForceEncoding(true);
		return characterEncodingFilter;
	}
}
