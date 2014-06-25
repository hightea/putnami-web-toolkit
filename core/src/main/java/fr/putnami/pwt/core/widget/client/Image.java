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

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.dom.client.Style;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Image extends AbstractWidget implements EditorOutput<String> {

	private static final String PROP_MAX_WIDTH = "maxWidth";
	private static final String PROP_MAX_HEIGHT = "maxHeight";
	private static final String PROP_WIDTH = "width";
	private static final String PROP_HEIGHT = "height";

	public enum Type implements CssStyle {
		DEFAULT(null),
		ROUNDED("img-rounded"),
		CIRCLE("img-circle"),
		THUMBNAIL("img-thumbnail");

		private final String style;

		private Type(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}


	private final ImageElement imgElement;

	private boolean keepPropertions = true;

	private Type type;
	private String src;
	private String alt;

	private int widthPx = -1;
	private int heightPx = -1;

	public Image() {
		super(ImageElement.TAG);
		imgElement = ImageElement.as(getElement());
	}

	protected Image(Image source) {
		super(source);
		imgElement = ImageElement.as(getElement());

		setSrc(source.src);
		setAlt(source.alt);
		this.widthPx = source.widthPx;
		this.widthPx = source.widthPx;
		this.keepPropertions = source.keepPropertions;

		resetSize();
	}

	@Override
	public IsWidget cloneWidget() {
		return new Image(this);
	}

	public String getSrc() {
		return src;
	}

	public void setSrc(String src) {
		this.src = src;
		if (this.src.startsWith("/")) {
			this.src = GWT.getModuleName() + this.src;
		}
		imgElement.setSrc(this.src);

	}

	public String getAlt() {
		return alt;
	}

	public void setAlt(String alt) {
		this.alt = alt;
		imgElement.setAlt(alt);
	}

	@Override
	public String getValue() {
		return src;
	}

	@Override
	public void edit(String value) {
		setSrc(value);
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
		StyleUtils.addStyle(this, type);
	}

	public boolean isKeepPropertions() {
		return keepPropertions;
	}

	public void setKeepPropertions(boolean keepPropertions) {
		this.keepPropertions = keepPropertions;
		resetSize();
	}

	public Integer getWidthPx() {
		return widthPx;
	}

	public void setWidthPx(Integer widthPx) {
		this.widthPx = widthPx;
		resetSize();
	}

	public Integer getHeightPx() {
		return heightPx;
	}

	public void setHeightPx(Integer heightPx) {
		this.heightPx = heightPx;
		resetSize();
	}

	private void resetSize() {
		Style imgStyle = imgElement.getStyle();
		imgStyle.clearProperty(PROP_WIDTH);
		imgStyle.clearProperty(PROP_HEIGHT);
		imgStyle.clearProperty(PROP_MAX_WIDTH);
		imgStyle.clearProperty(PROP_MAX_HEIGHT);

		if (widthPx > 0) {
			imgStyle.setPropertyPx(keepPropertions ? PROP_MAX_WIDTH : PROP_WIDTH, widthPx);
		}
		if (heightPx > 0) {
			imgStyle.setPropertyPx(keepPropertions ? PROP_MAX_HEIGHT : PROP_HEIGHT, heightPx);
		}
	}

}
