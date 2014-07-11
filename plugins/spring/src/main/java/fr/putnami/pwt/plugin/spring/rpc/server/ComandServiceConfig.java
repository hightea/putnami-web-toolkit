package fr.putnami.pwt.plugin.spring;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.i18n.CookieLocaleResolver;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;

import fr.putnami.pwt.core.security.shared.service.SessionService;
import fr.putnami.pwt.plugin.spring.rpc.server.controller.CommandServiceController;
import fr.putnami.pwt.plugin.spring.session.server.service.SessionServiceImpl;

@Configuration
@EnableWebMvc
public class PwtSpringConfig extends WebMvcConfigurerAdapter {

	@Override
	public void addInterceptors(InterceptorRegistry registry) {
		LocaleChangeInterceptor localeChangeInterceptor = new LocaleChangeInterceptor();
		localeChangeInterceptor.setParamName("locale");
		registry.addInterceptor(localeChangeInterceptor);
	}

	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {
		registry.addResourceHandler("/assets/**").addResourceLocations("/assets/").setCachePeriod(31556926);
		registry.addResourceHandler("/app/**").addResourceLocations("/").setCachePeriod(31556926);
	}

	@Bean
	public LocaleResolver localeResolver() {
		return new CookieLocaleResolver();

	}

	@Bean
	public CommandServiceController commandServiceController() {
		return new CommandServiceController();
	}

	@Bean
	public SessionService sessionService() {
		return new SessionServiceImpl();
	}

}
