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
package fr.putnami.pwt.core.theme.client;

import com.google.common.base.Objects;
import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.LinkElement;

/**
 * The Class CssLink wraps a {@link LinkElement} of one stylesheet.
 *
 * @since 1.0
 */
public class CssLink implements Comparable<CssLink> {

	private final int precedence;
	private final LinkElement link = Document.get().createLinkElement();

	/**
	 * Instantiates a new css link.
	 *
	 * @param href the link of the Css
	 * @param precedence the css, lower is the precedence value, higher is the priority.
	 */
	public CssLink(String href, int precedence) {
		super();
		this.precedence = precedence;
		this.link.setRel("stylesheet");
		this.link.setHref(GWT.getModuleBaseForStaticFiles() + href);
	}

	/**
	 * Gets the precedence.
	 * <p>
	 * lower is the precedence value, higher is the priority
	 * </p>
	 *
	 * @return the precedence
	 */
	public int getPrecedence() {
		return this.precedence;
	}

	/**
	 * Gets the link element of the css.
	 *
	 * @return the link element of the css
	 */
	public LinkElement getLink() {
		return this.link;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return Objects.hashCode(this.link.getHref());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CssLink) {
			CssLink other = (CssLink) obj;
			return Objects.equal(this.link.getHref(), other.link.getHref());
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(CssLink o) {
		if (o == null) {
			return 1;
		}
		return Integer.valueOf(this.precedence).compareTo(o.precedence);
	}
}
