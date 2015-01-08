package fr.putnami.pwt.core.service.client;

import com.google.common.base.Strings;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.MetaElement;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.http.client.RequestBuilder;

public class CsrfController {

	private static final String META_NAME_CSRF_TOKEN = "_csrf";
	private static final String META_NAME_CSRF_HEADER = "_csrf_header";

	private static CsrfController instance;

	public static CsrfController get() {
		if (instance == null) {
			instance = new CsrfController();
			instance.init();
		}
		return instance;
	}

	private String token;
	private String header = "X-CSRF-TOKEN";

	private CsrfController() {
	}

	private void init() {
		NodeList<Element> tags = Document.get().getElementsByTagName("meta");
		for (int i = 0; i < tags.getLength(); i++) {
			MetaElement metaTag = (MetaElement) tags.getItem(i);
			String metaName = metaTag.getName();
			String metaContent = metaTag.getContent();
			if (META_NAME_CSRF_TOKEN.equals(metaName) && !Strings.isNullOrEmpty(metaContent)) {
				this.token = metaContent;
			}
			if (META_NAME_CSRF_HEADER.equals(META_NAME_CSRF_HEADER) && !Strings.isNullOrEmpty(metaContent)) {
				this.header = metaContent;
			}
		}
	}

	public void securize(RequestBuilder rb) {
		if (token != null) {
			rb.setHeader(header, token);
		}
	}
}
