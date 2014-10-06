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

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import java.util.Collections;
import java.util.List;

/**
 * Theme helps to define the webapp's look.
 * <p>
 * By collecting {@link CssLink}s and {@link IconFont}, it manage all CSS of the webapp.
 * </p>
 * <p>
 * <strong>To create a new theme :</strong>
 * </p>
 *
 * <pre>
 * Theme yetiTheme = new Theme();
 * yetiTheme.addLink(new CssLink("theme/yeti/style/bootstrap-yeti.min.css", 0));
 * ThemeController.get().installTheme(yetiTheme);
 * </pre>
 *
 * @since 1.0
 */
public class Theme {

	private final List<CssLink> links = Lists.newArrayList();
	private IconFont iconFontCss;

	/**
	 * Adds a {@link CssLink} to the theme. The css precedence is declared within the {@link CssLink}.
	 *
	 * @param link the link
	 */
	public void addLink(CssLink link) {
		if (!this.links.contains(link)) {
			this.links.add(link);
		}
		Collections.sort(this.links);
	}

	/**
	 * Gets the links of the Theme. the links are wrapped in an unmodifiable Iterable.
	 *
	 * @return the links
	 */
	public Iterable<CssLink> getLinks() {
		return Iterables.unmodifiableIterable(this.links);
	}

	/**
	 * Gets the icon font installed in the Theme.
	 *
	 * @return the icon font
	 */
	public IconFont getIconFont() {
		return this.iconFontCss;
	}

	/**
	 * Sets the {@ling IconFont} to the theme.
	 *
	 * @param iconFont the new icon font
	 */
	public void setIconFont(IconFont iconFont) {
		this.iconFontCss = iconFont;
	}

}
