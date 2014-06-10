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

public interface HasResponsiveVisibility {

	enum XsVisibility implements CssStyle {
		DEFAULT(null),
		VISIBLE("visible-xs"),
		HIDDEN("hidden-xs");

		private final String style;

		private XsVisibility(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	enum SmVisibility implements CssStyle {
		DEFAULT(null),
		VISIBLE("visible-sm"),
		HIDDEN("hidden-sm");

		private final String style;

		private SmVisibility(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	enum MdVisibility implements CssStyle {
		DEFAULT(null),
		VISIBLE("visible-md"),
		HIDDEN("hidden-md");

		private final String style;

		private MdVisibility(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	enum LgVisibility implements CssStyle {
		DEFAULT(null),
		VISIBLE("visible-lg"),
		HIDDEN("hidden-lg");

		private final String style;

		private LgVisibility(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	enum PrintVisibility implements CssStyle {
		DEFAULT(null),
		VISIBLE("visible-print"),
		HIDDEN("hidden-print");

		private final String style;

		private PrintVisibility(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	void setXsVisibility(XsVisibility xsVisibility);

	void setSmVisibility(SmVisibility smVisibility);

	void setMdVisibility(MdVisibility mdVisibility);

	void setLgVisibility(LgVisibility lgVisibility);

	void setPrintVisibility(PrintVisibility printVisibility);
}
