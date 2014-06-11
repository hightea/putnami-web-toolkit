/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.theme.client;

import java.util.Collections;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.HeadElement;
import com.google.gwt.dom.client.LinkElement;
import com.google.gwt.dom.client.NodeList;

public class DefaultThemeController extends ThemeController {

	private HeadElement head;

	private final List<CssLink> defaultLinks = Lists.newArrayList();
	private Theme currentTheme;

	private boolean isInit = false;

	@Override
	public void addDefaultStyle(CssLink link) {
		if (!defaultLinks.contains(link)) {
			defaultLinks.add(link);
		}
	}

	@Override
	public void installTheme(Theme theme) {
		initThemeController();
		if (currentTheme != null) {
			for (CssLink link : currentTheme.getLinks()) {
				link.getLink().removeFromParent();
			}
		}
		resetDefault();
		this.currentTheme = theme;
		for (CssLink link : theme.getLinks()) {
			getHead().appendChild(link.getLink());
		}
	}

	private void initThemeController() {
		if (isInit) {
			return;
		}
		isInit = true;
		// Remove all existing link element
		NodeList<Element> links = getHead().getElementsByTagName(LinkElement.TAG);
		int size = links.getLength();
		for (int i = 0; i < size; i++) {
			links.getItem(0).removeFromParent();
		}
	}

	private void resetDefault() {
		Collections.sort(defaultLinks);
		for (CssLink link : defaultLinks) {
			getHead().appendChild(link.getLink());
		}
	}

	private HeadElement getHead() {
		if (head == null) {
			Element elt = Document.get().getElementsByTagName("head").getItem(0);
			assert elt != null : "The host HTML page does not have a <head> element"
					+ " which is required by this injector";
			head = HeadElement.as(elt);
		}
		return head;
	}

}
