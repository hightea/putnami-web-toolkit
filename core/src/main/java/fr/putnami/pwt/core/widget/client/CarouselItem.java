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
import com.google.gwt.uibinder.client.UiChild;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractHTMLPanel;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class CarouselItem extends AbstractHTMLPanel {

	private static final CssStyle STYLE_CAROUSEL_ITEM = new SimpleStyle("item");
	private static final CssStyle STYLE_CAROUSEL_CAPTION = new SimpleStyle("carousel-caption");

	public CarouselItem(String html) {
		super(DivElement.TAG, html);
		StyleUtils.addStyle(this, STYLE_CAROUSEL_ITEM);
	}

	@Override
	public IsWidget cloneWidget() {
		throw new java.lang.UnsupportedOperationException("CarouselItem cannot be cloned");
	}

	@UiChild(limit = 1, tagname = "caption")
	public void addCaption(IsWidget caption) {
		OneWidgetPanel captionContainer = new OneWidgetPanel();
		StyleUtils.addStyle(captionContainer, STYLE_CAROUSEL_CAPTION);
		captionContainer.setWidget(caption);
		add(captionContainer);
	}

}
