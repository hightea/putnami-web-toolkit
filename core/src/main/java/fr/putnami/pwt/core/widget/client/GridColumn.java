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
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.Iterator;
import java.util.List;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractHTMLPanel;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class GridColumn extends AbstractHTMLPanel {
	public static class SizeStyle implements CssStyle, Iterable<SizeStyle> {
		private final String preffix;
		private final int size;

		public SizeStyle(String preffix, int size) {
			super();
			assert size >= 0 && size <= 12 : "Invalid column size";

			this.preffix = preffix;
			this.size = size;
		}

		@Override
		public String get() {
			if (this.size == 0) {
				return null;
			}
			return this.preffix + this.size;
		}

		@Override
		public Iterator<SizeStyle> iterator() {
			List<SizeStyle> toRemove = Lists.newArrayList();
			for (int i = 1; i < 13; i++) {
				toRemove.add(new SizeStyle(this.preffix, i));
			}
			return toRemove.iterator();
		}
	}

	public static class OffsetStyle implements CssStyle, Iterable<OffsetStyle> {
		private final String preffix;
		private final int size;

		public OffsetStyle(String preffix, int size) {
			super();
			assert size >= 0 && size < 12 : "Invalid column size";

			this.preffix = preffix;
			this.size = size;
		}

		@Override
		public String get() {
			if (this.size == 0) {
				return null;
			}
			return this.preffix + this.size;
		}

		@Override
		public Iterator<OffsetStyle> iterator() {
			List<OffsetStyle> toRemove = Lists.newArrayList();
			for (int i = 1; i < 12; i++) {
				toRemove.add(new OffsetStyle(this.preffix, i));
			}
			return toRemove.iterator();
		}
	}

	public static final String PREFIX_SIZE_XS = "col-xs-";
	public static final String PREFIX_SIZE_SM = "col-sm-";
	public static final String PREFIX_SIZE_MD = "col-md-";
	public static final String PREFIX_SIZE_LG = "col-lg-";

	public static final String PREFIX_OFFSET_XS = "col-xs-offset-";
	public static final String PREFIX_OFFSET_SM = "col-sm-offset-";
	public static final String PREFIX_OFFSET_MD = "col-md-offset-";
	public static final String PREFIX_OFFSET_LG = "col-lg-offset-";

	private SizeStyle sizeXs = new SizeStyle(GridColumn.PREFIX_SIZE_XS, 0);
	private SizeStyle sizeSm = new SizeStyle(GridColumn.PREFIX_SIZE_SM, 0);
	private SizeStyle sizeMd = new SizeStyle(GridColumn.PREFIX_SIZE_MD, 12);
	private SizeStyle sizeLg = new SizeStyle(GridColumn.PREFIX_SIZE_LG, 0);
	private OffsetStyle offsetXs = new OffsetStyle(GridColumn.PREFIX_OFFSET_XS, 0);
	private OffsetStyle offsetSm = new OffsetStyle(GridColumn.PREFIX_OFFSET_SM, 0);
	private OffsetStyle offsetMd = new OffsetStyle(GridColumn.PREFIX_OFFSET_MD, 0);
	private OffsetStyle offsetLg = new OffsetStyle(GridColumn.PREFIX_OFFSET_LG, 0);

	public GridColumn() {
		this(AbstractHTMLPanel.EMPTY_HTML);
		this.redraw();
	}

	public GridColumn(String html) {
		super(DivElement.TAG, html);
		this.redraw();
	}

	protected GridColumn(GridColumn source) {
		super(source);

		this.offsetXs = source.offsetXs;
		this.offsetSm = source.offsetSm;
		this.offsetMd = source.offsetMd;
		this.offsetLg = source.offsetLg;
		this.sizeXs = source.sizeXs;
		this.sizeSm = source.sizeSm;
		this.sizeMd = source.sizeMd;
		this.sizeLg = source.sizeLg;

		this.redraw();
	}

	@Override
	public IsWidget cloneWidget() {
		return new GridColumn(this);
	}

	public void setSize(int size) {
		this.setSizeMd(size);
	}

	public void setOffset(int offset) {
		this.setOffsetMd(offset);
	}

	public int getOffsetXs() {
		return this.offsetXs.size;
	}

	public void setOffsetXs(int offset) {
		this.offsetXs = new OffsetStyle(GridColumn.PREFIX_OFFSET_XS, offset);
		this.redraw();
	}

	public int getOffsetSm() {
		return this.offsetSm.size;
	}

	public void setOffsetSm(int offset) {
		this.offsetSm = new OffsetStyle(GridColumn.PREFIX_OFFSET_SM, offset);
		this.redraw();
	}

	public int getOffsetMd() {
		return this.offsetMd.size;
	}

	public void setOffsetMd(int offset) {
		this.offsetMd = new OffsetStyle(GridColumn.PREFIX_OFFSET_MD, offset);
		this.redraw();
	}

	public int getOffsetLg() {
		return this.offsetLg.size;
	}

	public void setOffsetLg(int offset) {
		this.offsetLg = new OffsetStyle(GridColumn.PREFIX_OFFSET_LG, offset);
		this.redraw();
	}

	public int getSizeXs() {
		return this.sizeXs.size;
	}

	public void setSizeXs(int size) {
		this.sizeXs = new SizeStyle(GridColumn.PREFIX_SIZE_XS, size);
		this.redraw();
	}

	public int getSizeSm() {
		return this.sizeSm.size;
	}

	public void setSizeSm(int size) {
		this.sizeSm = new SizeStyle(GridColumn.PREFIX_SIZE_SM, size);
		this.redraw();
	}

	public int getSizeMd() {
		return this.sizeMd.size;
	}

	public void setSizeMd(int size) {
		this.sizeMd = new SizeStyle(GridColumn.PREFIX_SIZE_MD, size);
		this.redraw();
	}

	public int getSizeLg() {
		return this.sizeLg.size;
	}

	public void setSizeLg(int size) {
		this.sizeLg = new SizeStyle(GridColumn.PREFIX_SIZE_LG, size);
		this.redraw();
	}

	public void redraw() {
		StyleUtils.addStyle(this, this.offsetXs);
		StyleUtils.addStyle(this, this.offsetSm);
		StyleUtils.addStyle(this, this.offsetMd);
		StyleUtils.addStyle(this, this.offsetLg);
		StyleUtils.addStyle(this, this.sizeXs);
		StyleUtils.addStyle(this, this.sizeSm);
		StyleUtils.addStyle(this, this.sizeMd);
		StyleUtils.addStyle(this, this.sizeLg);
	}
}
