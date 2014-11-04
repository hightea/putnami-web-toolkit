package fr.putnami.pwt.sample.rest.client;

import org.fusesource.restygwt.client.Defaults;

import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.EntryPointHandler;
import fr.putnami.pwt.core.inject.client.annotation.ErrorManagmentDescription;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.sample.rest.client.application.PageDecorator;
import fr.putnami.pwt.sample.rest.client.welcome.WelcomePlace;

@MvpDescription(
	display = PageDecorator.class,
	defaultPlace = WelcomePlace.class,
	activities = {
		WelcomePlace.class
	})
@ErrorManagmentDescription(
	errorDisplay = SimpleErrorDisplayer.class)
public class RestSampleApp implements Module {

	@EntryPointHandler
	void setRestRootPath() {
		Defaults.setServiceRoot("http://reqr.es/");
	}
}
