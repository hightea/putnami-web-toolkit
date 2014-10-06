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
			this.container = Document.get().createDivElement();
			StyleUtils.addStyle(this.container, Tooltip.STYLE_TOOLTIP);
			StyleUtils.addStyle(this.container, Tooltip.STYLE_FADE);

			DivElement arrow = Document.get().createDivElement();
			StyleUtils.addStyle(arrow, Tooltip.STYLE_ARROW);
			this.container.appendChild(arrow);

			this.inner = Document.get().createDivElement();
			StyleUtils.addStyle(this.inner, Tooltip.STYLE_INNER);
			this.container.appendChild(this.inner);

			this.setElement(this.container);
		}

		public void setTooltip(String tooltip) {
			this.inner.setInnerText(tooltip);
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
		if (this.tooltipWidget == null) {
			this.tooltipWidget = new TooltipWidget();
		}
		this.tooltipWidget.setTooltip(this.text);
		return this.tooltipWidget;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {EditorLabel.LABEL_SUFFIX};
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		this.getHoverWidget();
		this.tooltipWidget.setTooltip(text);
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
