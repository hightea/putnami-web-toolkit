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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Text;
import com.google.gwt.uibinder.client.UiChild;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractOutput;

public class OutputBoolean extends AbstractOutput<Boolean> {

	public enum RenderType {
		ICON_AND_TEXT,
		ICON,
		TEXT;
	}

	private String trueLabel = Boolean.TRUE.toString();
	private String falseLabel = Boolean.TRUE.toString();

	private Icon trueIcon;
	private Icon falseIcon;

	private RenderType outputType = RenderType.ICON_AND_TEXT;

	public OutputBoolean() {
	}

	protected OutputBoolean(OutputBoolean source) {
		super(source);
		this.trueLabel = source.trueLabel;
		this.falseLabel = source.falseLabel;
		this.outputType = source.outputType;
		if (source.trueIcon != null) {
			setTrueIcon((Icon) source.trueIcon.cloneWidget());
		}
		if (source.falseIcon != null) {
			setFalseIcon((Icon) source.falseIcon.cloneWidget());
		}

	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputBoolean(this);
	}

	@Override
	protected void ensureElement(Element element) {
		resetInner();
	}

	@Override
	protected void renderValue(Boolean value) {
		resetInner();
	}

	private void resetInner() {
		if (elementExists()) {
			Element element = getElement();
			element.removeAllChildren();
			boolean rendervalue = Boolean.TRUE.equals(getValue());
			if (outputType != RenderType.TEXT) {
				Icon icon = rendervalue ? trueIcon : falseIcon;
				if (icon != null) {
					element.appendChild(icon.getElement());
				}
			}
			if (outputType != RenderType.ICON) {
				Text textElem = Document.get().createTextNode(rendervalue ? trueLabel : falseLabel);
				element.appendChild(textElem);
			}
		}
	}

	public String getTrueLabel() {
		return trueLabel;
	}

	public void setTrueLabel(String trueLabel) {
		this.trueLabel = trueLabel;
		resetInner();
	}

	public String getFalseLabel() {
		return falseLabel;
	}

	public void setFalseLabel(String falseLabel) {
		this.falseLabel = falseLabel;
		resetInner();
	}

	@UiChild(tagname = "iconTrue", limit = 1)
	public void setTrueIcon(Icon icon) {
		trueIcon = icon;
		resetInner();
	}

	public void setTrueIconType(String iconType) {
		trueIcon = new Icon();
		trueIcon.setType(iconType);
		resetInner();
	}

	@UiChild(tagname = "iconFalse", limit = 1)
	public void setFalseIcon(Icon icon) {
		falseIcon = icon;
		resetInner();
	}

	public void setFalseIconType(String iconType) {
		falseIcon = new Icon();
		falseIcon.setType(iconType);
		resetInner();
	}

	public void setOutputType(RenderType outputType) {
		this.outputType = outputType;
		resetInner();
	}

}
