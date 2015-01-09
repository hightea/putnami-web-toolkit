/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.service.client;

import com.google.common.base.Strings;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.MetaElement;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.http.client.RequestBuilder;

public final class CsrfController {

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

	public String getToken() {
		return token;
	}

	public String getHeader() {
		return header;
	}
}
