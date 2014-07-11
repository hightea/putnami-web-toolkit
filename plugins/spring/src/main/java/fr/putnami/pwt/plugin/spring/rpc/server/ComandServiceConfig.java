package fr.putnami.pwt.plugin.spring.rpc.server;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import fr.putnami.pwt.plugin.spring.rpc.server.controller.CommandServiceController;

@Configuration
public class ComandServiceConfig {
	@Bean
	public CommandServiceController commandServiceController() {
		return new CommandServiceController();
	}
}
