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

public class Theme {

	private final List<CssLink> links = Lists.newArrayList();
	private IconFont iconFontCss;

	public void addLink(CssLink link) {
		if (!this.links.contains(link)) {
			this.links.add(link);
		}
		Collections.sort(this.links);
	}

	public Iterable<CssLink> getLinks() {
		return Iterables.unmodifiableIterable(this.links);
	}

	public IconFont getIconFont() {
		return this.iconFontCss;
	}

	public void setIconFont(IconFont iconFontCss) {
		this.iconFontCss = iconFontCss;
	}

}
