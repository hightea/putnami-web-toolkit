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

import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.HasText;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractTextOutput<T> extends AbstractOutput<T> implements
		HasText {

	public enum Style implements CssStyle {
		DEFAULT(null),
		LABEL("label label-default"),
		LABEL_PRIMARY("label label-primary"),
		LABEL_SUCCESS("label label-success"),
		LABEL_INFO("label label-info"),
		LABEL_WARNING("label label-warning"),
		LABEL_DANGER("label label-danger"),
		BADGE("badge");

		private final String style;

		private Style(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	private Style style;

	private Renderer<T> renderer;

	private String text;

	public AbstractTextOutput() {
	}

	protected AbstractTextOutput(AbstractTextOutput<T> source) {
		super(source);
		renderer = source.renderer;
		style = source.style;
		text = source.text;
	}

	@Override
	protected void ensureElement(Element element) {
		if (text != null) {
			setText(text);
		}
		setStyle(style);
	}

	public void setRenderer(Renderer<T> renderer) {
		this.renderer = renderer;
		if (renderer != null) {
			setText(renderer.render(getValue()));
		}
	}

	@Override
	protected void renderValue(T value) {
		setText(renderer.render(value));
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		if (elementExists()) {
			getElement().setInnerText(text);
		}
	}

	public Style getStyle() {
		return style;
	}

	public void setStyle(Style style) {
		this.style = style;
		if (elementExists()) {
			StyleUtils.addStyle(getElement(), style);
		}
	}

}
