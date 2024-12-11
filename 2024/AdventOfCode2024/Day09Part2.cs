namespace AdventOfCode2024;

class Day09Part2
{
    public static void Run(string[] args)
    {
        var chars = Input.String().ToCharArray();
        var currentOffset = 0;
        var currentBlockId = 0;
        var blocks = new List<BlockInfo>();
        var freeSpace = new List<FreeSpace>();
        for (var i = 0; i < chars.Length; i += 2)
        {
            var blockSize = chars[i] - '0';
            blocks.Add(new BlockInfo
            {
                Id = currentBlockId,
                Offset = currentOffset,
                Size = blockSize,
            });
            currentBlockId += 1;
            currentOffset += blockSize;

            if (i + 1 < chars.Length)
            {
                var freeSpaceSize = chars[i + 1] - '0';
                if (freeSpaceSize > 0)
                {
                    freeSpace.Add(new FreeSpace
                    {
                        Offset = currentOffset,
                        Size = freeSpaceSize,
                    });
                    currentOffset += freeSpaceSize;
                }
            }
        }

        var targetBlockIndex = blocks.Count - 1;
        while (true)
        {
            var firstFreeSpace = freeSpace[0];
            var targetBlock = blocks[targetBlockIndex];
            if (firstFreeSpace.Offset >= targetBlock.Offset + targetBlock.Size)
            {
                break;
            }

            var suitableFreeSpaceIndex = freeSpace.FindIndex(s => s.Offset < targetBlock.Offset && s.Size >= targetBlock.Size);
            if (suitableFreeSpaceIndex < 0)
            {
                targetBlockIndex -= 1;
                continue;
            }
            var suitableFreeSpace = freeSpace[suitableFreeSpaceIndex];
            blocks.RemoveRange(targetBlockIndex, 1);
            targetBlock.Offset = suitableFreeSpace.Offset;
            var index = blocks.FindIndex(b => b.Offset > targetBlock.Offset);
            if (index >= 0)
            {
                blocks.Insert(index, targetBlock);
            }
            else
            {
                blocks.Add(targetBlock);
            }
            suitableFreeSpace.Offset += targetBlock.Size;
            suitableFreeSpace.Size -= targetBlock.Size;
            if (suitableFreeSpace.Size == 0)
            {
                freeSpace.RemoveRange(suitableFreeSpaceIndex, 1);
            }
        }

        foreach (var grouping in blocks.GroupBy(b => b.Id))
        {
            if (grouping.Count() > 1)
            {
                throw new Exception($"Block {grouping.Key} is fragmented.");
            }
        }
        var result = blocks.SelectMany(block => Enumerable.Range(block.Offset, block.Size).Select(i => i * block.Id)).Sum();

        Console.WriteLine("{0}", result);
    }

    record BlockInfo
    {
        public long Id { get; init; }
        public int Offset { get; set; }
        public int Size { get; init; }

        public override string ToString() => $"{{id {Id}, offset {Offset}, size {Size}}}";
    }

    record FreeSpace
    {
        public int Offset { get; set; }
        public int Size { get; set; }

        public override string ToString() => $"{{offset {Offset}, size {Size}}}";
    }
}
