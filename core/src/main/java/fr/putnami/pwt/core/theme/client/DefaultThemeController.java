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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.HeadElement;
import com.google.gwt.dom.client.LinkElement;
import com.google.gwt.dom.client.NodeList;

import fr.putnami.pwt.core.widget.client.base.SimpleStyle;

public class DefaultThemeController extends ThemeController {

	private HeadElement head;

	private final Theme defaultTheme = new Theme();
	private Theme currentTheme;
	private IconFont icons;

	private boolean isInit = false;

	@Override
	public Theme getDefaultTheme() {
		return defaultTheme;
	}

	@Override
	public void installTheme(Theme theme) {
		removeCssLinks();
		if (currentTheme != null) {
			for (CssLink link : currentTheme.getLinks()) {
				link.getLink().removeFromParent();
			}
		}
		this.currentTheme = theme;
		resetTheme();
	}

	@Override
	public void resetTheme() {
		insertLinks(this.defaultTheme);
		insertLinks(this.currentTheme);
		if (currentTheme != null && currentTheme.getIconFont() != null) {
			icons = currentTheme.getIconFont();
		}
		else if (defaultTheme.getIconFont() != null) {
			icons = defaultTheme.getIconFont();
		}
		if (icons != null) {
			getHead().appendChild(icons.getLink());
		}
	}

	private void insertLinks(Theme theme) {
		if (theme != null) {
			for (CssLink link : theme.getLinks()) {
				getHead().appendChild(link.getLink());
			}
		}
	}

	private void removeCssLinks() {
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

	private HeadElement getHead() {
		if (head == null) {
			Element elt = Document.get().getElementsByTagName("head").getItem(0);
			assert elt != null : "The host HTML page does not have a <head> element"
					+ " which is required by this injector";
			head = HeadElement.as(elt);
		}
		return head;
	}

	@Override
	public CssStyle getIconStyle(String iconName) {
		if (icons == null) {
			return SimpleStyle.EMPTY_STYLE;
		}
		return icons.getStyle(iconName);
	}

}
