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

import com.google.common.collect.Lists;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.user.client.ui.HasHTML;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.List;

import fr.putnami.pwt.core.widget.client.base.AbstractWidget;

public class Heading extends AbstractWidget implements HasHTML {

	public static final List<String> HEADING_TAGS = Lists.newArrayList("h1", "h2", "h3", "h4", "h5",
		"h6");
	public static final String ATTRIBUTE_DATA_SUMMARY = "data-summary";

	private static final int HEADER_MINIMUM = 1;

	private static final int HEADER_MAXIMUM = 6;

	private final int level;

	private String text;
	private String small;
	private String html;

	@UiConstructor
	public Heading(int level) {
		super("h" + level);
		this.level = level;
		if (level < Heading.HEADER_MINIMUM || level > Heading.HEADER_MAXIMUM) {
			throw new IllegalArgumentException("The size of the header must be between 1 and 6.");
		}
	}

	private Heading(Heading source) {
		super(source);
		this.level = source.level;
		this.text = source.text;
		this.small = source.small;
		this.html = source.html;
		this.redraw();
	}

	@Override
	public IsWidget cloneWidget() {
		return new Heading(this);
	}

	public int getLevel() {
		return this.level;
	}

	public void setSubtext(String subtext) {
		this.small = subtext;
		this.redraw();
	}

	public String getSubtext() {
		return this.small;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		this.redraw();
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public String getHTML() {
		return this.html;
	}

	@Override
	public void setHTML(String html) {
		this.html = html;
		this.text = html;
		this.redraw();
	}

	private void redraw() {
		if (this.text != null) {
			this.html = this.text;
			if (this.small != null) {
				this.html += " <small>" + this.small + "</small>";
			}
		}
		this.getElement().setInnerHTML(this.html);
	}

	public void setSummary(String summary) {
		this.getElement().setAttribute(Heading.ATTRIBUTE_DATA_SUMMARY, summary);
	}
}
