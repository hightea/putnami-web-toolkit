package fr.putnami.pwt.plugin.ajaxbot;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

import fr.putnami.pwt.plugin.ajaxbot.controller.SiteMapController;
import fr.putnami.pwt.plugin.ajaxbot.filter.AjaxPageFilter;

@Configuration
public class AjaxBotIndexingConfig {

	@Bean
	public SiteMapController siteMapController() {
		return new SiteMapController();
	}

	@Bean
	@Order(Ordered.HIGHEST_PRECEDENCE + 1)
	public AjaxPageFilter ajaxBotIndexingFilter() {
		return new AjaxPageFilter();
	}
}
