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
package fr.putnami.pwt.core.widget.client.base;

import java.util.Iterator;
import java.util.List;

import com.google.common.collect.Lists;

import fr.putnami.pwt.core.theme.client.CssStyle;

public interface HasResponsiveVisibility {

	enum TargetSize {
		XS("xs"),
		SM("sm"),
		MD("md"),
		LG("lg"),
		PRINT("print");

		private final String value;

		private TargetSize(String value) {
			this.value = value;
		}

		public String value() {
			return value;
		}
	}

	enum Visibility {
		DEFAULT(null, null),
		HIDDEN("hidden-", null),
		VISIBLE_BLOCK("visible-", "-block"),
		VISIBLE_INLINE("visible-", "-inline"),
		VISIBLE_INLINE_BLOCK("visible-", "-inline-block");

		private final String prefix;
		private final String suffix;

		private Visibility(String prefix, String suffix) {
			this.prefix = prefix;
			this.suffix = suffix;
		}

		public String get(TargetSize size) {
			if (this == DEFAULT) {
				return null;
			}
			String result = prefix + size.value();
			if (suffix != null) {
				result += suffix;
			}
			return result;
		}
	}

	public static class VisibilityStyle implements CssStyle, Iterable<VisibilityStyle> {
		private final TargetSize size;
		private final Visibility visibility;

		public VisibilityStyle(TargetSize size, Visibility visibility) {
			this.size = size;
			this.visibility = visibility;
		}

		@Override
		public String get() {
			return visibility.get(size);
		}

		@Override
		public Iterator<VisibilityStyle> iterator() {
			List<VisibilityStyle> toRemove = Lists.newArrayList();
			for (Visibility availableVisibility : Visibility.values()) {
				toRemove.add(new VisibilityStyle(size, availableVisibility));
			}
			return toRemove.iterator();
		}
	}

	void setXsVisibility(Visibility xsVisibility);

	void setSmVisibility(Visibility smVisibility);

	void setMdVisibility(Visibility mdVisibility);

	void setLgVisibility(Visibility lgVisibility);

	void setPrintVisibility(Visibility printVisibility);
}
