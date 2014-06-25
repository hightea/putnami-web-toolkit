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

import java.util.Iterator;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.user.client.ui.IsWidget;

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
			if (size == 0) {
				return null;
			}
			else {
				return preffix + size;
			}
		}

		@Override
		public Iterator<SizeStyle> iterator() {
			List<SizeStyle> toRemove = Lists.newArrayList();
			for (int i = 1; i < 13; i++) {
				toRemove.add(new SizeStyle(preffix, i));
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
			if (size == 0) {
				return null;
			}
			else {
				return preffix + size;
			}
		}

		@Override
		public Iterator<OffsetStyle> iterator() {
			List<OffsetStyle> toRemove = Lists.newArrayList();
			for (int i = 1; i < 12; i++) {
				toRemove.add(new OffsetStyle(preffix, i));
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

	private SizeStyle sizeXs = new SizeStyle(PREFIX_SIZE_XS, 0);
	private SizeStyle sizeSm = new SizeStyle(PREFIX_SIZE_SM, 0);
	private SizeStyle sizeMd = new SizeStyle(PREFIX_SIZE_MD, 12);
	private SizeStyle sizeLg = new SizeStyle(PREFIX_SIZE_LG, 0);
	private OffsetStyle offsetXs = new OffsetStyle(PREFIX_OFFSET_XS, 0);
	private OffsetStyle offsetSm = new OffsetStyle(PREFIX_OFFSET_SM, 0);
	private OffsetStyle offsetMd = new OffsetStyle(PREFIX_OFFSET_MD, 0);
	private OffsetStyle offsetLg = new OffsetStyle(PREFIX_OFFSET_LG, 0);

	public GridColumn() {
		this(EMPTY_HTML);
		redraw();
	}

	public GridColumn(String html) {
		super(DivElement.TAG, html);
		redraw();
	}

	protected GridColumn(GridColumn source) {
		super(source);

		offsetXs = source.offsetXs;
		offsetSm = source.offsetSm;
		offsetMd = source.offsetMd;
		offsetLg = source.offsetLg;
		sizeXs = source.sizeXs;
		sizeSm = source.sizeSm;
		sizeMd = source.sizeMd;
		sizeLg = source.sizeLg;

		redraw();
	}

	@Override
	public IsWidget cloneWidget() {
		return new GridColumn(this);
	}

	public void setSize(int size) {
		setSizeMd(size);
	}

	public void setOffset(int offset) {
		setOffsetMd(offset);
	}

	public int getOffsetXs() {
		return offsetXs.size;
	}

	public void setOffsetXs(int offset) {
		this.offsetXs = new OffsetStyle(PREFIX_OFFSET_XS, offset);
		redraw();
	}

	public int getOffsetSm() {
		return offsetSm.size;
	}

	public void setOffsetSm(int offset) {
		this.offsetSm = new OffsetStyle(PREFIX_OFFSET_SM, offset);
		redraw();
	}

	public int getOffsetMd() {
		return offsetMd.size;
	}

	public void setOffsetMd(int offset) {
		this.offsetMd = new OffsetStyle(PREFIX_OFFSET_MD, offset);
		redraw();
	}

	public int getOffsetLg() {
		return offsetLg.size;
	}

	public void setOffsetLg(int offset) {
		this.offsetLg = new OffsetStyle(PREFIX_OFFSET_LG, offset);
		redraw();
	}

	public int getSizeXs() {
		return sizeXs.size;
	}

	public void setSizeXs(int size) {
		this.sizeXs = new SizeStyle(PREFIX_SIZE_XS, size);
		redraw();
	}

	public int getSizeSm() {
		return sizeSm.size;
	}

	public void setSizeSm(int size) {
		this.sizeSm = new SizeStyle(PREFIX_SIZE_SM, size);
		redraw();
	}

	public int getSizeMd() {
		return sizeMd.size;
	}

	public void setSizeMd(int size) {
		this.sizeMd = new SizeStyle(PREFIX_SIZE_MD, size);
		redraw();
	}

	public int getSizeLg() {
		return sizeLg.size;
	}

	public void setSizeLg(int size) {
		this.sizeLg = new SizeStyle(PREFIX_SIZE_LG, size);
		redraw();
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
