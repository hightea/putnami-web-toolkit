package fr.putnami.pwt.doc.client.page.sample.decorator;

import com.google.common.collect.Multimap;

public interface HasSources {
	String VIEW_PANEL = "Views";
	String CONSTANTS_PANEL = "Constants";
	String DOMAIN_PANEL = "Domain";
	String SERVICE_PANEL = "InjectService";

	Multimap<String, String> getSourcesMap();
}
