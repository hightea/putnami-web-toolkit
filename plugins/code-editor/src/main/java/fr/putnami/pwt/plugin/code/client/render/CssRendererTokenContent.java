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
package fr.putnami.pwt.plugin.code.client.render;

import com.google.common.base.Objects;

import fr.putnami.pwt.plugin.code.client.token.TokenContent;

public class CssRendererTokenContent implements TokenContent {

	public static final CssRendererTokenContent DEFAULT_CSS_TOKEN_CONTENT =
		new CssRendererTokenContent(null);

	private String cssStyle;

	public CssRendererTokenContent(String cssStyle) {
		this.cssStyle = cssStyle;
	}

	public String getCssStyle() {
		return this.cssStyle;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof CssRendererTokenContent) {
			return Objects.equal(this.getCssStyle(), ((CssRendererTokenContent) other).getCssStyle());
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(this.cssStyle);
	}

}
