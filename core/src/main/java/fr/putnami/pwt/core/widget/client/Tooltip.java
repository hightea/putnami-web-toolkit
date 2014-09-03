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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractHover;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Tooltip extends AbstractHover implements EditorLabel {

	private static final CssStyle STYLE_TOOLTIP = new SimpleStyle("tooltip");
	private static final CssStyle STYLE_ARROW = new SimpleStyle("tooltip-arrow");
	private static final CssStyle STYLE_INNER = new SimpleStyle("tooltip-inner");
	private static final CssStyle STYLE_FADE = new SimpleStyle("fade");

	public static class TooltipWidget extends Widget {

		private DivElement container;
		private DivElement inner;

		public TooltipWidget() {
			container = Document.get().createDivElement();
			StyleUtils.addStyle(container, STYLE_TOOLTIP);
			StyleUtils.addStyle(container, STYLE_FADE);

			DivElement arrow = Document.get().createDivElement();
			StyleUtils.addStyle(arrow, STYLE_ARROW);
			container.appendChild(arrow);

			inner = Document.get().createDivElement();
			StyleUtils.addStyle(inner, STYLE_INNER);
			container.appendChild(inner);

			this.setElement(container);
		}

		public void setTooltip(String tooltip) {
			inner.setInnerText(tooltip);
		}
	}

	private TooltipWidget tooltipWidget;
	private String text;

	public Tooltip() {
		super();
	}

	private Tooltip(Tooltip source) {
		super(source);
		this.text = source.text;
	}

	@Override
	public IsWidget cloneWidget() {
		return new Tooltip(this);
	}

	@Override
	protected Widget getHoverWidget() {
		if (tooltipWidget == null) {
			tooltipWidget = new TooltipWidget();
		}
		tooltipWidget.setTooltip(text);
		return tooltipWidget;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {
				LABEL_SUFFIX
		};
	}

	@Override
	public String getText() {
		return text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		getHoverWidget();
		tooltipWidget.setTooltip(text);
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public boolean isLabelMandatory() {
		return false;
	}

}
